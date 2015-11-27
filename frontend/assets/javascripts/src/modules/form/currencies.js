define(['$', 'bonzo'], function ($, bonzo) {
    'use strict';

    var CURRENCY_ATTR = "data-currency";
    var BILLING_PERIOD_ATTR = "data-billing-period";
    var PAYMENT_OPTIONS_CONTAINER_ELEM = document.querySelector('.js-payment-options-container');
    var SUBMIT_BTN = document.querySelector('.js-submit-input');
    var defaultBillingPeriod = "Year";

    function show(elem) {
        $(elem).removeClass("is-hidden");
    }

    function hide(elem) {
        $(elem).addClass("is-hidden");
    }

    function children(elem) {
        return Array.prototype.slice.call(elem.childNodes);
    }

    function dataSelector(attr, value) {
        return "[" + attr + "=" + value + "]";
    }

    function showPaymentOptions(currency) {
        children(PAYMENT_OPTIONS_CONTAINER_ELEM.)

    }

    function showSubmitButtonPrice(currency, billingPeriod) {
        children(SUBMIT_BTN).forEach(function (el){
            var $el = $(el);
            console.info(el);
            if ($el.attr(CURRENCY_ATTR) === currency && $el.attr(BILLING_PERIOD_ATTR) === billingPeriod) {
                show($el)
            }
        });
    }

    function displayCurrency() {
        var checkoutForm = guardian.membership.checkoutForm;
        showPaymentOption(checkoutForm.currency);

        if (SUBMIT_BTN && !checkoutForm.lockCurrency) {
            showSubmitButtonPrice(checkoutForm.currency, defaultBillingPeriod);
        }
    }

    return {
        CURRENCY_ATTR: CURRENCY_ATTR,
        init: displayCurrency
    };
});
