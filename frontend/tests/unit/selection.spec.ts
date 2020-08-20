import { getNodeOffset, getOffsetNode } from "@/utils/selection";

describe("getNodeOffset()", () => {
  it("returns 0 if the searched node is the parent", () => {
    const parent = document.createElement("div");
    expect(getNodeOffset(parent, parent)).toBe(0);
  });

  it("returns 0 if the searched node is the first child", () => {
    const parent = document.createElement("div");
    const node = document.createElement("p");
    parent.appendChild(node);
    expect(getNodeOffset(parent, node)).toBe(0);
  });

  it("returns the size of preceding elements if the searched node is not the first child", () => {
    const parent = document.createElement("div");

    const text1 = document.createTextNode("abc");
    parent.appendChild(text1);

    const p2 = document.createElement("p");
    p2.textContent = "def";
    parent.appendChild(p2);

    const span3 = document.createElement("span");
    span3.textContent = "hij";
    parent.appendChild(span3);

    const node = parent.appendChild(document.createElement("p"));

    const text4 = document.createTextNode("klm");
    parent.appendChild(text4);

    expect(getNodeOffset(parent, node)).toBe(9);
  });

  it("also works if the searched node is nested", () => {
    const parent = document.createElement("div");

    const text1 = document.createTextNode("abc");
    parent.appendChild(text1);

    const span2 = document.createElement("span");
    span2.textContent = "def";
    parent.appendChild(span2);

    const node = span2.appendChild(document.createElement("p"));
    expect(getNodeOffset(parent, node)).toBe(6);
  });
});

describe("getOffsetNode()", () => {
  it("returns the parent at 0 if the searched offset is 0", () => {
    const parent = document.createElement("div");
    expect(getOffsetNode(parent, 0)).toStrictEqual({ node: parent, offset: 0 });
  });

  it("returns the text item and the offset when the only child is a text item", () => {
    const parent = document.createElement("div");

    const text = document.createTextNode("abc");
    parent.appendChild(text);

    expect(getOffsetNode(parent, 1)).toStrictEqual({ node: text, offset: 1 });
  });

  it("returns the correct text item and the remaining offset when there are multiple text items", () => {
    const parent = document.createElement("div");

    const text1 = document.createTextNode("abc");
    parent.appendChild(text1);

    const text2 = document.createTextNode("def");
    parent.appendChild(text2);

    expect(getOffsetNode(parent, 5)).toStrictEqual({ node: text2, offset: 2 });
  });

  it("points at the end of the parent if the offset is larger than the size of the children", () => {
    const parent = document.createElement("div");

    const text = document.createTextNode("abc");
    parent.appendChild(text);

    expect(getOffsetNode(parent, 3)).toStrictEqual({ node: text, offset: 3 });
    expect(getOffsetNode(parent, 5)).toStrictEqual({ node: text, offset: 3 });
  });
});
