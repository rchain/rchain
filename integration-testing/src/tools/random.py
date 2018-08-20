import random
import string

def random_string(length):
    return ''.join(random.choice(string.ascii_letters) for m in range(length))