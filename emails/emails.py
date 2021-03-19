import smtplib
import ssl
import csv
import email.mime.multipart as emM
import email.mime.text as emT

directory = "/".join(__file__.split("\\")[:-1])

restrict = ("None", False)
subject = "Thank you for Signing Up for the Newsletter!"
text = "".join(x.strip() for x in list(open(directory + "/welcome_email.html", "r")))

# For obvious reasons, the "key.txt" file is not put in the github repository.
# It is the file that contains the password for the email account.
try:
    password = list(open(directory + "/key.txt", "r"))[0].strip()
    file = open(directory + "/key.txt", "w")
    file.close()
except FileNotFoundError:
    print("You don't have the password.")
    exit(-1)
except Exception as e:
    print(e.__class__.__name__ + " error occured in the reading of the key file.")
    exit(-1)

sender = "newelectoralcollege@gmail.com"

sent_to = []

context = ssl.create_default_context()
message = emM.MIMEMultipart("alternative")
message["Subject"], message["From"] = subject, "The New Electoral College"
message.attach(emT.MIMEText(text, "html"))

with smtplib.SMTP_SSL("smtp.gmail.com", 465, context=context) as server:
    server.login(sender, password)

    # The "emails.csv" is also not in the github repository
    with open(directory + "/emails.csv", "r") as file:
        lines = csv.reader(file)
        for first, last, state, email, admin in lines:
            if (restrict[0] in [state, "None"]) and (restrict[1] and admin or not(restrict[1])) and not(email in sent_to):
                message["To"] = email
                server.sendmail(sender, email, message.as_string().format(first=first, last=last, state=state, email=email))
                sent_to.append(email)

    server.quit()