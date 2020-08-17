export const getNodeOffset = (parent: Node, node: Node): number => {
  if (parent === node) {
    return 0;
  }

  let offset = 0;
  const children = parent.childNodes;
  for (let i = 0; i < children.length; ++i) {
    const child = children[i];
    if (child === node) {
      return offset;
    } else if (
      child.compareDocumentPosition(node) & Node.DOCUMENT_POSITION_CONTAINED_BY
    ) {
      return offset + getNodeOffset(child, node);
    } else if (
      child.nodeType === Node.TEXT_NODE ||
      child.nodeType === Node.ELEMENT_NODE
    ) {
      offset += (child.textContent || "").length;
    } else {
      console.error("Unexpected child node: ", child);
    }
  }
  console.error("Did not find node under parent", parent, node);
  return 0;
};

export const getOffsetNode = (
  parent: Node,
  offset: number
): { node: Node; offset: number } => {
  if (offset === 0) {
    return { node: parent, offset: 0 };
  }

  if (parent.nodeType === Node.TEXT_NODE) {
    return {
      node: parent,
      offset: Math.min(offset, (parent.textContent || "").length),
    };
  }

  let remainingOffset = offset;
  const children = parent.childNodes;
  for (let i = 0; i < children.length - 1; ++i) {
    const child = children[i];
    const childTextLength = (child.textContent || "").length;
    if (childTextLength < remainingOffset) {
      remainingOffset -= childTextLength;
    } else {
      return getOffsetNode(child, remainingOffset);
    }
  }
  return getOffsetNode(children[children.length - 1], remainingOffset);
};

export const getSelectionPosition = (parent: Node) => {
  const currentSelection = document.getSelection();

  // a selection must exist
  if (currentSelection === null) {
    return null;
  }

  const { anchorNode, anchorOffset, focusNode, focusOffset } = currentSelection;
  // and it must have an anchor and a focus
  if (
    anchorNode === null ||
    anchorOffset === null ||
    focusNode === null ||
    focusOffset === null
  ) {
    return null;
  }

  // and both anchor an focus are inside of `div`
  if (
    !(
      parent.compareDocumentPosition(anchorNode) &
        Node.DOCUMENT_POSITION_CONTAINED_BY &&
      parent.compareDocumentPosition(focusNode) &
        Node.DOCUMENT_POSITION_CONTAINED_BY
    )
  ) {
    return null;
  }

  const start = getNodeOffset(parent, anchorNode) + anchorOffset;
  const end = getNodeOffset(parent, focusNode) + focusOffset;
  return { start, end };
};

export const setSelectedPosition = (
  parent: Node,
  start: number,
  end: number
) => {
  const { node: anchorNode, offset: anchorOffset } = getOffsetNode(
    parent,
    start
  );
  const { node: focusNode, offset: focusOffset } = getOffsetNode(parent, end);

  if (
    anchorNode === null ||
    anchorOffset === null ||
    focusNode === null ||
    focusOffset === null
  ) {
    return;
  }

  console.log(anchorNode, anchorOffset, focusNode, focusOffset);

  document
    .getSelection()
    ?.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
};
