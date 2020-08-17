import { getNodeOffset, getOffsetNode } from "@/utils/selection";

describe("getNodeOffset()", () => {
  it("returns 0 if the searched node is the parent", () => {
    let parent = document.createElement("div");
    expect(getNodeOffset(parent, parent)).toBe(0);
  });

  it("returns 0 if the searched node is the first child", () => {
    let parent = document.createElement("div");
    let node = document.createElement("p");
    parent.appendChild(node);
    expect(getNodeOffset(parent, node)).toBe(0);
  });

  it("returns the size of preceding elements if the searched node is not the first child", () => {
    let parent = document.createElement("div");

    let text1 = document.createTextNode("abc");
    parent.appendChild(text1);

    let p2 = document.createElement("p");
    p2.textContent = "def";
    parent.appendChild(p2);

    let span3 = document.createElement("span");
    span3.textContent = "hij";
    parent.appendChild(span3);

    let node = parent.appendChild(document.createElement("p"));

    let text4 = document.createTextNode("klm");
    parent.appendChild(text4);

    expect(getNodeOffset(parent, node)).toBe(9);
  });

  it("also works if the searched node is nested", () => {
    let parent = document.createElement("div");

    let text1 = document.createTextNode("abc");
    parent.appendChild(text1);

    let span2 = document.createElement("span");
    span2.textContent = "def";
    parent.appendChild(span2);

    let node = span2.appendChild(document.createElement("p"));
    expect(getNodeOffset(parent, node)).toBe(6);
  });
});
