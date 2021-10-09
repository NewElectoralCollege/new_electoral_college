from flask import Flask, redirect, jsonify
import stripe
from flask_cors import CORS, cross_origin

port = 50
servername = 'localhost' + ':' + str(port)
domain = 'http://' + servername

app = Flask(__name__)
CORS(app, resources={r"/create-checkout-session": {"origins": domain}})


@app.route('/create-checkout-session', methods=['POST'])
@cross_origin(origin='localhost', headers=['Content-Type', 'application/json'])
def create_checkout_session():
    try:
        intent = stripe.PaymentIntent.create(
            amount=200,
            currency='usd',
            api_key='sk_test_51Jhf7CCOIWLLuzJ63rseG0KMZPQDG3rLdUPJpIEANRrYZTL7nuXxtqzYMdd7V3ftOFRKhKy4L4JY5CQjC6dt4UcN00xrwDEaWo'
        )
        return jsonify({
            'clientSecret': intent['client_secret']
        })

    except Exception as e:
        return jsonify(error=str(e)), 403


if __name__ == '__main__':
    app.config['SERVER_NAME'] = servername
    app.config['DOMAIN'] = domain
    app.run(port=port)
