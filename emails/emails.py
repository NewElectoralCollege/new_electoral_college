import smtplib
import csv
import email.mime.multipart as emM
import email.mime.text as emT
import os

def send(subject, file_name, welcome=[]):

    restrict = ("None", False)

    text = "".join(x.strip()
                   for x in list(open("./" + file_name, "r")))

    sender = os.environ["FROM_ADDRESS"]
    password = os.environ["PASSWORD"]

    sent_to = []

    message = emM.MIMEMultipart("alternative")
    message["Subject"], message["From"] = subject, "The New Electoral College"
    message.attach(emT.MIMEText(text, "html"))

    try:
        server = smtplib.SMTP("smtp.gmail.com", 587)
        server.starttls()
        server.login(sender, password)

        if len(welcome) > 0:
            message["To"] = welcome[3]
            server.sendmail(sender, welcome[3], message.as_string().format(
                first=welcome[0], last=welcome[1], state=welcome[2], email=welcome[3]))
        else:
            with open("./emails.csv", "r") as file:
                lines = csv.reader(file)
                for first, last, state, email, admin in lines:
                    if (restrict[0] in [state, "None"]) and (restrict[1] and admin or not(restrict[1])) and not(email in sent_to):
                        message["To"] = email
                        server.sendmail(sender, email, message.as_string().format(
                            first=first, last=last, state=state, email=email))
                        sent_to.append(email)

        server.quit()
    except Exception as e:
        print(e)


if __name__ == "__main__":
    send()
