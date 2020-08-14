<template>
  <textarea
    class="editor"
    spellcheck="false"
    v-model="formula"
    v-on:input="debounceListener"
  />

  <div
    class="editor disabled"
    contenteditable="false"
    spellcheck="false"
    v-html="styledFormula"
  ></div>

  <div>
    <span v-if="computedResult.value !== undefined">{{
      computedResult.value
    }}</span>
    <span v-else-if="computedResult.error !== undefined" class="error">{{
      computedResult.error.message
    }}</span>
    <span v-else>...</span>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed, ref, Ref, watch } from "vue";

import gql from "graphql-tag";

import { ApolloClient } from "apollo-client";
import { createHttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";

const httpLink = createHttpLink({
  uri: "http://localhost:8100/graphql",
});

const cache = new InMemoryCache();

const apolloClient = new ApolloClient({
  link: httpLink,
  cache,
});

interface Error {
  message: string;
  offset: number;
}

interface Result {
  input: string;
  value?: string;
  error?: Error;
}

export default defineComponent({
  name: "Calculator",
  setup: () => {
    const formula = ref("");
    const debouncedFormula = ref("");

    const computedResult: Ref<Result> = ref({
      input: formula.value,
      value: "",
    });

    const styledFormula = computed(() => {
      let formula = computedResult.value.input;
      if (computedResult.value.error !== undefined) {
        let offset = computedResult.value.error.offset;
        if (offset == -1) {
          offset = formula.length;
          formula = formula + " ";
        }
        const result =
          formula.slice(0, offset) +
          "<span style='color: red; text-decoration: underline; white-space: pre;'>" +
          formula.slice(offset, offset + 1) +
          "</span>" +
          formula.slice(offset + 1);
        return result;
      } else {
        return formula;
      }
    });

    const formulaAsText = computed(() => {
      const div = document.createElement("div");
      div.innerHTML = styledFormula.value;
      return div.textContent || div.innerText || "";
    });

    let timeoutRef: NodeJS.Timeout | null = null;
    const debounceListener = () => {
      if (timeoutRef !== null) {
        clearTimeout(timeoutRef);
      }

      timeoutRef = setTimeout(() => {
        debouncedFormula.value = formula.value;
      }, 800);
    };

    watch(debouncedFormula, (newFormula) => {
      computedResult.value = { input: debouncedFormula.value };

      apolloClient
        .query({
          query: gql`
            query Calculate($input: String!) {
              calculate(input: $input)
            }
          `,
          variables: { input: newFormula },
        })
        .then((response) => {
          computedResult.value = {
            input: debouncedFormula.value,
            value: response.data.calculate,
          };
        })
        .catch((error) => {
          if (error.graphQLErrors.length == 1) {
            const gqlError = error.graphQLErrors[0];
            computedResult.value = {
              input: debouncedFormula.value,
              error: {
                message: gqlError.message,
                offset: gqlError.extensions.offset,
              },
            };
          } else if (error.networkError !== undefined) {
            computedResult.value = {
              input: debouncedFormula.value,
              error: error.networkError,
            };
          } else {
            computedResult.value = { input: debouncedFormula.value, error };
          }
        });
    });

    return {
      debounceListener,
      formula,
      computedResult,
      styledFormula,
    };
  },
});
</script>

<style scoped lang="scss">
.error {
  color: red;
}

.editor {
  font: inherit;
  border: 1px solid grey;
  height: 100px;
  width: 300px;
  padding: 10px;
}

.disabled {
  background-color: #e2e2e2;
}
</style>
