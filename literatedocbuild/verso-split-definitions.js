(() => {
  const definitionBinding = "kw-occ-Lean.Parser.Command.definition-";
  let bodyIndex = 0;
  const definitions = [];

  const isDefinition = (node) =>
    node instanceof HTMLElement &&
    node.matches("code.hl.lean.block") &&
    node.querySelector(`.keyword[data-binding^="${definitionBinding}"]`);

  const isDocstring = (node) =>
    node instanceof HTMLElement &&
    node.matches(".verso-text, .md-text");

  const hasVisibleContent = (node) => (node.textContent || "").trim() !== "";

  const splitDefinition = (code) => {
    const walker = document.createTreeWalker(code, NodeFilter.SHOW_TEXT);
    let newlineNode = null;
    let newlineIndex = -1;

    while (walker.nextNode()) {
      const index = walker.currentNode.data.indexOf("\n");
      if (index >= 0) {
        newlineNode = walker.currentNode;
        newlineIndex = index;
        break;
      }
    }

    code.classList.add("function-signature");
    if (!newlineNode) return { signature: code, body: null };

    const body = code.cloneNode(false);
    body.classList.remove("function-signature");
    body.classList.add("function-body");

    const bodyRange = document.createRange();
    bodyRange.setStart(newlineNode, newlineIndex + 1);
    bodyRange.setEnd(code, code.childNodes.length);
    body.append(bodyRange.extractContents());
    newlineNode.deleteData(newlineIndex, 1);

    return { signature: code, body };
  };

  const makeBodyFoldable = (signature, body) => {
    const bodyId = `function-body-${bodyIndex++}`;
    body.id = bodyId;
    signature.classList.add("function-body-toggle");
    signature.tabIndex = 0;
    signature.setAttribute("role", "button");
    signature.setAttribute("aria-controls", bodyId);
    signature.setAttribute("aria-expanded", "false");
    signature.title = "Click to show the function body";
    body.hidden = true;

    const toggleBody = () => {
      body.hidden = !body.hidden;
      signature.setAttribute("aria-expanded", String(!body.hidden));
      signature.title = body.hidden
        ? "Click to show the function body"
        : "Click to hide the function body";
    };

    signature.addEventListener("click", (event) => {
      if (event.target.closest("a")) return;
      toggleBody();
    });

    signature.addEventListener("keydown", (event) => {
      if (event.target !== signature || (event.key !== "Enter" && event.key !== " ")) return;
      event.preventDefault();
      toggleBody();
    });
  };

  for (const original of document.querySelectorAll(".code-content > .code-box")) {
    if (!Array.from(original.children).some(isDefinition)) continue;

    const output = document.createDocumentFragment();
    let context = document.createElement("div");
    context.className = "code-box";

    const flushContext = () => {
      if (hasVisibleContent(context)) output.append(context);
      context = document.createElement("div");
      context.className = "code-box";
    };

    for (const node of Array.from(original.childNodes)) {
      if (!isDefinition(node)) {
        context.append(node);
        continue;
      }

      let docstring = null;
      const previous = Array.from(context.children).at(-1);
      if (isDocstring(previous)) {
        docstring = previous;
        previous.remove();
      }

      flushContext();

      const definitionBox = document.createElement("div");
      definitionBox.className = "code-box";
      if (docstring) {
        docstring.classList.add("declaration-doc");
        output.append(docstring);
      }
      const { signature, body } = splitDefinition(node);
      const declaration = signature.querySelector(".const[id]");
      if (declaration) {
        definitions.push({
          id: declaration.id,
          name: declaration.textContent.trim(),
        });
      }
      definitionBox.append(signature);
      if (body) {
        makeBodyFoldable(signature, body);
        definitionBox.append(document.createTextNode("\n"), body);
      }
      output.append(definitionBox);
    }

    flushContext();
    original.replaceWith(output);
  }

  const addDefinitionIndex = () => {
    if (definitions.length === 0) return;

    let toc = document.querySelector(".page-toc");
    if (!toc) {
      const codeContent = document.querySelector(".code-content");
      if (!codeContent?.parentElement) return;
      toc = document.createElement("nav");
      toc.className = "page-toc";
      toc.setAttribute("aria-label", "Page table of contents");
      codeContent.after(toc);
    }

    const title = document.createElement("div");
    title.className = "page-toc-title definition-index-title";
    title.textContent = "Definitions";

    const list = document.createElement("ul");
    list.className = "definition-index";
    const seen = new Set();
    for (const definition of definitions) {
      if (seen.has(definition.id)) continue;
      seen.add(definition.id);

      const item = document.createElement("li");
      item.className = "toc-level-2";
      const link = document.createElement("a");
      const url = new URL(window.location.href);
      url.hash = definition.id;
      link.href = url.href;
      link.textContent = definition.name;
      item.append(link);
      list.append(item);
    }

    toc.append(title, list);
  };

  addDefinitionIndex();

  const dropEmptyCodeBoxes = () => {
    for (const box of document.querySelectorAll(".code-content > .code-box")) {
      const content = Array.from(box.childNodes)
        .filter(
          (node) =>
            !(node instanceof HTMLElement && node.matches(".copy-button"))
        )
        .map((node) => node.textContent || "")
        .join("")
        .trim();
      if (content === "") box.remove();
    }
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", dropEmptyCodeBoxes);
  } else {
    dropEmptyCodeBoxes();
  }
})();
