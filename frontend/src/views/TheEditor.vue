<template>
  <div>
    <div class="container">
      <div class="row">
        <div class="col-12"><Editor /></div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent } from "vue";
import Editor from "@/components/Editor.vue";

export default defineComponent({
  name: "TheEditor",
  components: {
    Editor,
  },
});
</script>
