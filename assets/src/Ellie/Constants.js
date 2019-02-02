export default {
  publicPath: () => {
    return document.head.querySelector("meta[name='public_path']").content;
  }
};
