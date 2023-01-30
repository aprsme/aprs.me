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

let csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");
let liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
});

// Show progress bar on live navigation and form submits
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (info) =>
  topbar.delayedShow(200)
);
window.addEventListener("phx:page-loading-stop", (info) => topbar.hide());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;

window.addEventListener("phx:page-loading-stop", (_info) => {
  let data = {
    markersByCallsign: {},
    polylinesByCallsign: {},
    recentCallsigns: [],
    mapZoom: 10,
  };

  window.data = data;

  // map stuff here
  const MAX_ZOOM = 20;

  // let map = L.map("map", {
  //   minZoom: 1,
  //   maxZoom: MAX_ZOOM,
  //   worldCopyJump: true,
  //   keyboard: true,
  // }).setView([mapLatitude, mapLongitude], 12);

  var map = L.map("map").setView([51.505, -0.09], 13);

  let resizeMap = () => {
    const height = $(window).height();
    const width = $(window).width();

    document.querySelector("#map").height(height).width(width);
    map.invalidateSize();
  };

  // $(window)
  //   .on("resize", () => {
  //     resizeMap();
  //   })
  //   .trigger("resize");

  L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution:
      '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  }).addTo(map);

  // let markerGroup = L.markerClusterGroup({
  //   removeOutsideVisibleBounds: true,
  //   disableClusteringAtZoom: 8,
  // });

  // map.addLayer(markerGroup);
});
