define(['$', 'bean'], function ($, bean) {
    'use strict';

    var CURRENCY_ATTR = "data-currency";
    var DATA_CURRENCTY_SEL = "[" + CURRENCY_ATTR + "]";

    var PAYMENT_OPTIONS_CONTAINER_ELEM = document.querySelector('.js-payment-options-container');
    var FORM_SUBMIT_PRICE_OPTION_ELEM = document.querySelector('.js-submit-price-option');
    var CARD_DETAILS_NOTE_ELEM = document.querySelector('.js-card-details-note');

    var CARD_NOTE_CURRENCIES = CARD_DETAILS_NOTE_ELEM.querySelectorAll(DATA_CURRENCTY_SEL);

    var $CARD_NOTE_PERIOD_ELEMS = $('.js-card-note-pricing-period');
    var CARD_NOTE_PAYMENT_TAKEN_ELEM = CARD_DETAILS_NOTE_ELEM.querySelector('.js-card-note-payment-taken');
    var SUBMIT_BTN = document.querySelector('.js-submit-input');


    var checkoutForm = guardian.membership.checkoutForm;

    var init = function () {
        if (PAYMENT_OPTIONS_CONTAINER_ELEM && checkoutForm) {
            var currency = "USD"; //checkoutForm && checkoutForm.currency || "GBP";
            showPaymentOptions(currency);
            addListeners();
            populateCardNote(currency, PAYMENT_OPTIONS_CONTAINER_ELEM.querySelector('[checked]').value);
        }
    };

    /**
     * add listeners for payment options
     */
    var addListeners = function () {
        bean.on(PAYMENT_OPTIONS_CONTAINER_ELEM, 'click', 'input', function (e) {
            var input = e && e.target;
            var currency = input.parentNode.parentNode.getAttribute(CURRENCY_ATTR);

            populateCardNote(currency, input.value);
            //var paymentOptionPrice = input.getAttribute('data-pricing-option-amount');
            //console.info("setting payment option price", paymentOptionPrice);
            //FORM_SUBMIT_PRICE_OPTION_ELEM.textContent = paymentOptionPrice;
        });
    };

    function toArray(nodeList) {
      return Array.prototype.slice.call(nodeList);
    }

    function children(elem) {
        return toArray(elem.childNodes).filter(function (el) {
            return el.nodeType == 1;
        });
    }

    function toggleCurrency(currency, testEl, targetEl) {
        targetEl = targetEl || testEl;
        if ($(testEl).attr(CURRENCY_ATTR) === currency) {
            targetEl.classList.remove("is-hidden");
            targetEl.setAttribute("disabled", false);
        } else {
            targetEl.classList.add("is-hidden");
            targetEl.setAttribute("disabled", true);
        }
    }

    function showPaymentOptions(currency) {
        toArray(PAYMENT_OPTIONS_CONTAINER_ELEM.querySelectorAll(DATA_CURRENCTY_SEL)).forEach(function (el) {
            toggleCurrency(currency, el);
        });
    }

    /**
     * display different information dependant on chosen payment option
     * @param currency
     * @param period
     */
    var populateCardNote = function(currency, period) {
        toArray(CARD_NOTE_CURRENCIES).forEach(function (el) {
            toggleCurrency(currency, el);
        });

        var attr = 'data-' + period;
        CARD_NOTE_PAYMENT_TAKEN_ELEM.innerHTML = CARD_NOTE_PAYMENT_TAKEN_ELEM.getAttribute(attr);
        $CARD_NOTE_PERIOD_ELEMS.text($CARD_NOTE_PERIOD_ELEMS.attr(attr));
    };

    return {
        init: init
    };
});
