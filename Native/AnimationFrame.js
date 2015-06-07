Elm.Native.AnimationFrame = {};
Elm.Native.AnimationFrame.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.AnimationFrame = localRuntime.Native.AnimationFrame || {};
  if (localRuntime.Native.AnimationFrame.values) return localRuntime.Native.AnimationFrame.values;

  var Signal = Elm.Signal.make(localRuntime);
  var NS = Elm.Native.Signal.make(localRuntime);

  // TODO Should be localRuntime.requestAnimationFrame, and should be shimmed if we care
  // about IE9. Do we care about IE9?
  var requestAnimationFrame = window.requestAnimationFrame || function () {};
  var cancelAnimationFrame = window.cancelAnimationFrame || function () {};

  function frameWhen(predicate, sourceSignal) {
    var prev = 0, curr = 0, wasOn = true;
    var ticker = NS.input("frameWhen", curr);
    function tick(curr) {
      curr = new Date().getTime();
      localRuntime.notify(ticker.id, curr);
    }
    var rafID = 0;
    function f(sourceValue, time) {
      var isOn = A2(predicate, time, sourceValue);
      if (isOn) {
        if (rafID == 0) console.log("Starting animation frames");
        rafID = requestAnimationFrame(tick);
      } else if (wasOn) {
        console.log("Stopping animation frames");
        cancelAnimationFrame(rafID);
        rafID = 0;
      }
      wasOn = isOn;
      return time;
    }
    return A3(Signal.map2, F2(f), sourceSignal, ticker);
  }

  return localRuntime.Native.AnimationFrame.values = {
    frameWhen : F2(frameWhen)
  };

};