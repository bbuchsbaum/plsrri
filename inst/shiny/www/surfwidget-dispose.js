(function () {
  if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) return;

  // Dispose neurosurf surfwidget WebGL resources by calling the widget's
  // exported `destroy()` method (see neurosurf/htmlwidgets/surfwidget.js).
  Shiny.addCustomMessageHandler("dispose_surfwidget", function (message) {
    try {
      var id = null;
      if (typeof message === "string") {
        id = message;
      } else if (message && typeof message.id === "string") {
        id = message.id;
      }
      if (!id) return;

      if (typeof HTMLWidgets !== "undefined" && typeof HTMLWidgets.find === "function") {
        var widget = HTMLWidgets.find("#" + id);
        if (widget && typeof widget.destroy === "function") {
          widget.destroy();
          return;
        }
      }

      // Fallback: clear the output container.
      var el = document.getElementById(id);
      if (el) el.innerHTML = "";
    } catch (e) {
      // Keep failure non-fatal; disposal is best-effort.
      console.warn("dispose_surfwidget failed:", e);
    }
  });
})();

