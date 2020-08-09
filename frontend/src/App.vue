<template>
  <div id="app">
    <div class="mt-4 mb-4">
      <router-view />
    </div>
  </div>
</template>

<script lang="ts">
import { Options, Vue } from "vue-class-component";

export default class App extends Vue {}
</script>
