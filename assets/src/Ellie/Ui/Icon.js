import CustomElements from "../../Platform/CustomElements";

export default {
  start(app) {
    import(/* webpackChunkName: "icons-svg" */ "./Icon.svg").then(text => {
      CustomElements.define(
        "ellie-ui-icon-sprite",
        HTMLElement =>
          class extends HTMLElement {
            connectedCallback() {
              this.style.display = "none";
              this.innerHTML = text;
            }
          }
      );
    });
  }
};
