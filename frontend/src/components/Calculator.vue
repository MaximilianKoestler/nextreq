<template>
  <div
    class="editor"
    contenteditable="true"
    spellcheck="false"
    @input="onInputChange($event)"
  ></div>

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
  start: number;
  end: number;
}

interface Result {
  input: string;
  value?: string;
  error?: Error;
}

export default defineComponent({
  name: "Calculator",
  setup: () => {
    const input = ref("");
    const debouncedPureInput = ref("");

    const pureInput = computed(() => {
      const div = document.createElement("div");
      div.innerHTML = input.value;
      return div.textContent || div.innerText || "";
    });

    let timeout: NodeJS.Timeout | null = null;
    const onInputChange = (e: any) => {
      input.value = e.target.innerHTML;

      if (timeout !== null) {
        clearTimeout(timeout);
      }
      timeout = setTimeout(() => {
        debouncedPureInput.value = input.value;
      }, 800);
    };

    const computedResult: Ref<Result> = ref({
      input: input.value,
      value: "",
    });

    const styledFormula = computed(() => {
      let formula = computedResult.value.input;
      if (computedResult.value.error !== undefined) {
        let start = computedResult.value.error.start;
        if (start == -1) {
          start = formula.length;
          formula = formula + " ";
        }

        let end = computedResult.value.error.end;
        if (end == -1) {
          end = formula.length;
        }

        const result =
          formula.slice(0, start) +
          "<span class='inline-error'>" +
          formula.slice(start, end) +
          "</span>" +
          formula.slice(end);
        return result;
      } else {
        return formula;
      }
    });

    watch(debouncedPureInput, (newInput) => {
      computedResult.value = { input: newInput };

      apolloClient
        .query({
          query: gql`
            query Calculate($input: String!) {
              calculate(input: $input)
            }
          `,
          variables: { input: newInput },
        })
        .then((response) => {
          computedResult.value = {
            input: newInput,
            value: response.data.calculate,
          };
        })
        .catch((error) => {
          if (error.graphQLErrors.length == 1) {
            const gqlError = error.graphQLErrors[0];
            computedResult.value = {
              input: newInput,
              error: {
                message: gqlError.message,
                start: gqlError.extensions.start,
                end: gqlError.extensions.end,
              },
            };
          } else if (error.networkError !== undefined) {
            computedResult.value = {
              input: newInput,
              error: error.networkError,
            };
          } else {
            computedResult.value = { input: newInput, error };
          }
        });
    });

    return {
      onInputChange,
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

<style lang="scss">
.inline-error {
  text-decoration: underline wavy red;
  white-space: pre;
  text-decoration-skip-ink: none;
}
</style>
