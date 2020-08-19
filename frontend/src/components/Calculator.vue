<template>
  <div>
    <div
      ref="editorInput"
      class="editor"
      contenteditable="true"
      spellcheck="false"
      @input="onInputChange($event)"
    ></div>

    <div class="result">
      <span v-if="computedResult.value !== undefined">{{
        computedResult.value
      }}</span>
      <span v-else-if="computedResult.error !== undefined" class="error">{{
        computedResult.error.message
      }}</span>
      <span v-else>...</span>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref, Ref, watch, onMounted } from "vue";

import gql from "graphql-tag";
import { ApolloClient } from "apollo-client";
import { createHttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";

import { getSelectionPosition, setSelectedPosition } from "@/utils/selection";

const httpLink = createHttpLink({
  uri: process.env.VUE_APP_API_ROOT_URL,
});

const cache = new InMemoryCache();

const apolloClient = new ApolloClient({
  link: httpLink,
  cache,
});

interface Error {
  message: string;
  start?: number;
  end?: number;
}

interface Result {
  input: string;
  value?: string;
  error?: Error;
}

const escapeText = (text: string) => {
  const div = document.createElement("div");
  div.appendChild(document.createTextNode(text));
  return div.innerHTML;
};

const htmlToPlain = (html: string) => {
  const div = document.createElement("div");
  div.innerHTML = html;
  return div.textContent || div.innerText || "";
};

const addErrorStyle = (text: string, start: number, end: number) => {
  const styledText =
    escapeText(text.slice(0, start)) +
    "<span class='inline-error'>" +
    escapeText(text.slice(start, end)) +
    "</span>" +
    escapeText(text.slice(end));
  return styledText;
};

const updateDiv = (div: HTMLDivElement, html: string) => {
  const selection = getSelectionPosition(div);
  div.innerHTML = html;
  if (selection !== null) {
    const { start, end } = selection;
    setSelectedPosition(div, start, end);
  }
};

export default defineComponent({
  name: "Calculator",
  setup: () => {
    const input = ref("");
    const debouncedPureInput = ref("");
    const editorInput = ref(null);
    const computedResult: Ref<Result> = ref({
      input: input.value,
      value: "",
    });

    onMounted(() => {
      const editorDiv: HTMLDivElement | null = editorInput.value;
      if (editorDiv !== null) {
        (editorDiv as HTMLDivElement).focus();
      }
    });

    let timeout: NodeJS.Timeout | null = null;
    const onInputChange = (e: Event) => {
      input.value = htmlToPlain((e.target as HTMLDivElement).innerHTML);

      if (timeout !== null) {
        clearTimeout(timeout);
      }
      timeout = setTimeout(() => {
        debouncedPureInput.value = input.value;
      }, 800);
    };

    // apply style to input in case of error
    watch(computedResult, (newResult, oldResult) => {
      if (newResult.error === oldResult.error) {
        return;
      }

      const editorDiv: HTMLDivElement | null = editorInput.value;
      if (editorDiv === null) {
        return;
      }

      const currentInput = input.value;
      if (newResult.error !== undefined) {
        const error = newResult.error;
        const oldInput = newResult.input;

        if (error.start !== undefined && error.end !== undefined) {
          let start = error.start;
          if (start == -1) {
            start = oldInput.length - 1;
          }

          let end = error.end;
          if (end == -1) {
            end = oldInput.length;
          }

          const styledInput = addErrorStyle(currentInput, start, end);
          updateDiv(editorDiv, styledInput);
        }
      } else {
        updateDiv(editorDiv, escapeText(currentInput));
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
              error: { message: "" + error.networkError },
            };
          } else {
            computedResult.value = {
              input: newInput,
              error: { message: "" + error },
            };
          }
        });
    });

    return {
      onInputChange,
      computedResult,
      editorInput,
    };
  },
});
</script>

<style scoped lang="scss">
.error {
  color: red;
}

.editor {
  border: 1px solid black;
  border-radius: 4px;
  padding: 0.5rem;
  width: 100%;
  height: 100px;
}

.result {
  border: 1px solid #888888;
  border-radius: 4px;

  background-color: #eeeeee;

  margin-top: 1rem;
  padding: 0.5rem;
  width: 100%;
  height: 75px;
}
</style>

<style lang="scss">
.inline-error {
  text-decoration: underline red;
  white-space: pre;
  text-decoration-skip-ink: none;
}
</style>
