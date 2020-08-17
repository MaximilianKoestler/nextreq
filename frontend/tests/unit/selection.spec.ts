import { getNodeOffset, getOffsetNode } from "@/utils/selection";

describe("getNodeOffset()", () => {
  it("returns 0 if the searched node is the parent", () => {
    let parent = document.createElement("div");
    expect(getNodeOffset(parent, parent)).toBe(0);
  });
});
