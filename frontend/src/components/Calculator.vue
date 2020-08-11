<template>
  <input type="text" v-model="formula" v-on:input="debounceListener" />
  <div>
    <span v-if="computedResult.value !== undefined">{{
      computedResult.value
    }}</span>
    <span v-else-if="computedResult.error !== undefined" class="error">{{
      computedResult.error
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

interface Result {
  value?: string;
  error?: string;
}

export default defineComponent({
  name: "Calculator",
  setup: () => {
    const formula = ref("");
    const debouncedFormula = ref("");

    const computedResult: Ref<Result> = ref({ value: "" });

    let timeoutRef: NodeJS.Timeout | null = null;
    const debounceListener = (e: any) => {
      if (timeoutRef !== null) {
        clearTimeout(timeoutRef);
      }

      formula.value = e.target.value;
      timeoutRef = setTimeout(() => {
        debouncedFormula.value = e.target.value;
      }, 800);
    };

    watch(debouncedFormula, (newFormula) => {
      computedResult.value = {};

      apolloClient
        .query({
          query: gql`
          query {
            calculate(input: "${newFormula}")
          }
        `,
        })
        .then((response) => {
          computedResult.value = { value: response.data.calculate };
        })
        .catch((error) => {
          if (error.graphQLErrors.length == 1) {
            computedResult.value = { error: error.graphQLErrors[0].message };
          } else if (error.networkError !== undefined) {
            computedResult.value = { error: error.networkError };
          } else {
            computedResult.value = { error };
          }
        });
    });

    return {
      debounceListener,
      formula,
      computedResult,
    };
  },
});
</script>

<style scoped lang="scss">
.error {
  color: red;
}
</style>
