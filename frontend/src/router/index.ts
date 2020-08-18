import { createRouter, createWebHashHistory, RouteRecordRaw } from "vue-router";

import TheCalculator from "@/views/TheCalculator.vue";
import TheEditor from "@/views/TheEditor.vue";

const routes: Array<RouteRecordRaw> = [
  {
    path: "/",
    redirect: "/calculator",
  },
  {
    path: "/calculator",
    name: "calculator",
    component: TheCalculator,
  },
  {
    path: "/editor",
    name: "editor",
    component: TheEditor,
  },
];

const router = createRouter({
  history: createWebHashHistory(),
  routes,
});

export default router;
