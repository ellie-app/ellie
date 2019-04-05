export default {
  start(app) {
    const isSubscribedKeyCombo = e => {
      return (
        ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === "Enter") ||
        ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === "p") ||
        ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === "d") ||
        ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === "o") ||
        ((e.metaKey || e.ctrlKey) && e.key === ",") ||
        ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === "r") ||
        ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key === "l") ||
        ((e.metaKey || e.ctrlKey) && e.key === "s")
      );
    };

    let queue = [];
    let callback = null;

    window.addEventListener("keydown", e => {
      if (e.key === "Meta" || e.key === "Control" || e.key === "Shift") return;
      if (!isSubscribedKeyCombo(e)) return;
      e.preventDefault();
      queue.push(e);
      cancelIdleCallback(callback);
      callback = requestIdleCallback(() => {
        queue.forEach(app.ports.effectProgramKeyDowns.send);
        queue = [];
      });
    });
  }
};
