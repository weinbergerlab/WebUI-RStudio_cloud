// Stepper input bindings
var stepperBinding = new Shiny.InputBinding();

$.extend(stepperBinding, {
  find: function(scope) {
    return $(scope).find("ul.stepper");
  },

  initialize: function(el){
    this.setValue(el, $(el).data("stepper-selected"));
  },
  
  getValue: function(el) {
    return $(el).data("stepper-selected");
  },

  setValue: function(el, value) {
    var step = $(el).children("li#" + value);
    this.setStep($(el), step);
  },

  setStep: function(el, step) {
    if (step) {
      el.data("stepper-selected", step.attr("id"));
      el.children("li").not(step).removeClass("active");
      step.addClass("active");
      el.trigger("stepper:selection-changed", [step.attr("id")]);
    } else {
      el.data("stepper-selected", null);
      el.children("li").removeClass("active");
      el.trigger("stepper:selection-changed", null);
    }
  },

  subscribe: function(el, callback) {   
    var bindings = this;
    $(el).find("> li > a").on('click.mdb-stepper', function(event) {
      var step = $(event.target).closest("ul.stepper > li");
      if (step.hasClass("active")) {
        bindings.setStep($(el), null);
      } else {
        bindings.setStep($(el), step);
      }
      callback(false);
    });
  },
  
  unsubscribe: function(el, callback) {   
      $(el).children("li > a").off('.mdb-stepper');
  }
});

Shiny.inputBindings.register(stepperBinding);

Shiny.addCustomMessageHandler("md_update_stepper", function(message) {
  if (message.value) {
    stepperBinding.setValue($("#" + message.stepper), message.value);
  }
});

Shiny.addCustomMessageHandler("md_update_stepper_step", function(message) {
  if ("enabled" in message) {
    $("#" + message.stepper).children("#" + message.step).toggleClass("completed", message.enabled);
  }
});

Shiny.addCustomMessageHandler("md_update_spinner", function(message) {
  if ("hidden" in message) {
    $("#" + message.spinner).toggle(!message.hidden);
  }
  if ("visible" in message) {
    $("#" + message.spinner).toggleClass("invisible", !message.visible);
  }
});

