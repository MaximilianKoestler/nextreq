<template>
  <div id="app">
    <TheNavbar />
    <div class="mt-4 mb-4">
      <router-view />
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent } from "vue";

import TheNavbar from "@/components/TheNavbar.vue";

export default defineComponent({
  name: "App",
  components: {
    TheNavbar,
  },
});
</script>

<style lang="scss">
body {
  font-family: Arial, Verdana, sans-serif;
}
</style>
