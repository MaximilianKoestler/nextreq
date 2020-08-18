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
import hljs from "highlight.js";

import "@/assets/styles/github.css";

marked.setOptions({
  highlight: function (code, lang, callback) {
    let highlighted = code;
    try {
      if (hljs.getLanguage(lang) !== undefined) {
        highlighted = hljs.highlight(lang, code).value;
      } else if (lang === "") {
        highlighted = hljs.highlightAuto(code).value;
      }
    } catch (e) {
      console.error(e);
      return code;
    }

    if (callback !== undefined) {
      callback(null, highlighted);
    } else {
      return highlighted;
    }
  },
});

export default defineComponent({
  name: "Editor",
  setup: () => {
    const markdownInput = ref("");

    const onMarkdownChange = (e: Event) => {
      markdownInput.value = (e.target as HTMLDivElement).innerText;
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
