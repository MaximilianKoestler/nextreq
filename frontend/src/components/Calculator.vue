<template>
  <input type="text" v-model="formula" />
  <div>{{ computedResult }}</div>
</template>

<script lang="ts">
import { defineComponent, ref, watch } from "vue";

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

export default defineComponent({
  name: "Calculator",
  setup: () => {
    const formula = ref("");
    const computedResult = ref("");

    watch(formula, (newFormula) => {
      const result = apolloClient.query({
        query: gql`
          query {
            calculate(input: "${newFormula}")
          }
        `,
      });
      result
        .then((response) => {
          computedResult.value = response.data.calculate;
        })
        .catch((error) => {
          console.error(error);
        });
    });

    return {
      formula,
      computedResult,
    };
  },
});
</script>

<style scoped lang="scss"></style>
