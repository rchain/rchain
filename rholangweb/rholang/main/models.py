from django.db import models

class Log(models.Model):
    input = models.TextField(blank=True)
    output = models.TextField(blank=True)

    def __str__(self):
        return str(self.input) + str(self.output)

