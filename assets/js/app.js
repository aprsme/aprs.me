// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");

// Import minimal APRS map hook
import MapAPRSMap from "./map";

// Responsive Slideover Hook
let ResponsiveSlideoverHook = {
  mounted() {
    this.isInitialized = false;

    this.handleResize = () => {
      const isDesktop = window.innerWidth >= 1024;
      const isMobile = window.innerWidth < 1024;

      // Set initial state based on screen size
      if (!this.isInitialized) {
        this.isInitialized = true;
        if (isDesktop) {
          this.pushEvent("set_slideover_state", { open: true });
        } else {
          this.pushEvent("set_slideover_state", { open: false });
        }
      }
    };

    // Initial check after a brief delay to ensure LiveView is ready
    setTimeout(() => {
      this.handleResize();
    }, 100);

    // Listen for resize events with debouncing
    let resizeTimer;
    this.debouncedResize = () => {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(this.handleResize, 150);
    };

    window.addEventListener("resize", this.debouncedResize);
  },

  destroyed() {
    if (this.debouncedResize) {
      window.removeEventListener("resize", this.debouncedResize);
    }
  },
};

// APRS Map Hook
let Hooks = {};
Hooks.APRSMap = MapAPRSMap;
Hooks.ResponsiveSlideoverHook = ResponsiveSlideoverHook;

let liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});

// Show progress bar on live navigation and form submits
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (info) => topbar.delayedShow(200));
window.addEventListener("phx:page-loading-stop", (info) => topbar.hide());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;
