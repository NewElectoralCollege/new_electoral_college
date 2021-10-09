const key =
    "pk_test_51Jhf7CCOIWLLuzJ6iRL1ILaxbtgSeYrC7vZKhcMpK1bSMIdlSl9Ua4gWhZO0NtZVC07mFa2TkvWmFIVkxm1wnOFe00dw2OJvTx";

const stripe = Stripe(key);

window.onload = function () {
    const forms = $("#payment-form");

    let elements = stripe.elements();

    let cardnumber = elements.create("cardNumber");
    let cardexpiry = elements.create("cardExpiry");
    let cardcvc = elements.create("cardCvc");

    cardnumber.mount("#card-number");
    cardexpiry.mount("#card-expiry");
    cardcvc.mount("#card-cvc");

    elements.getElement("cardNumber").update({ showIcon: true });

    Array.prototype.filter.call(forms, function (form) {
        forms
            .on("submit", validate_form(form))
            .on("submit", async function (event) {
                fetch("http://localhost:50/create-checkout-session", {
                    method: "POST",
                })
                    .then((response) => response.json())
                    .then(function (data) {
                        stripe
                            .confirmCardPayment(data.clientSecret, {
                                payment_method: { card: card },
                            })
                            .then(function (ccp) {
                                console.log(ccp);
                            });
                    });

                event.preventDefault();
            });
    });
};
