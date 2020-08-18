<template>
  <div
    ref="markdownInput"
    class="editor markdown"
    contenteditable="true"
    spellcheck="true"
    @input="onMarkdownChange($event)"
  ></div>

  <div
    ref="editorPreview"
    class="editor preview"
    contenteditable="false"
    spellcheck="true"
    v-html="preview"
  ></div>
</template>

<script lang="ts">
import { defineComponent, ref, computed } from "vue";

import marked from "marked";

export default defineComponent({
  name: "Editor",
  setup: () => {
    const markdownInput = ref("");

    const onMarkdownChange = (e: Event) => {
      markdownInput.value = (e.target as HTMLDivElement).innerText;
      console.log(markdownInput.value);
    };

    const preview = computed(() => {
      return marked(markdownInput.value);
    });

    return {
      onMarkdownChange,
      preview,
    };
  },
});
</script>

<style scoped lang="scss">
.editor {
  border: 1px solid black;
  border-radius: 4px;
  padding: 0.5rem;
  width: 100%;
  min-height: 100px;
}

.markdown {
  font-family: "Roboto Mono", monospace;
}

.preview {
  border: 1px solid #888888;
  background-color: #eeeeee;

  margin-top: 1rem;
}
</style>
