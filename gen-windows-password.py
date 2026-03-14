import secrets
import string

def gen_password(length=16):
    symbols = "!@#$%^&*()_+=-"
    alphabet = string.ascii_letters + string.digits + symbols

    while True:
        pw = ''.join(secrets.choice(alphabet) for _ in range(length))

        classes = sum([
            any(c.isupper() for c in pw),
            any(c.islower() for c in pw),
            any(c.isdigit() for c in pw),
            any(c in symbols for c in pw),
        ])

        if classes >= 3:
            return pw

print(gen_password())
