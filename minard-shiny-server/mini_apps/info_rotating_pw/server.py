from etext import send_sms_via_email
from os import environ

def send_pw(pw):

  message = pw
  phone_number = environ["PN"]
  provider = environ["PROV"]

  sender_credentials = (environ[EMAIL], environ["EMAIL_PW"])

  send_sms_via_email(
    phone_number, message, provider, sender_credentials, subject = "current password"
  )

  return(True)
