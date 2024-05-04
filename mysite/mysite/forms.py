from django import forms 
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User

class SignupForm(UserCreationForm):
    class Meta:
        model = User 
        fields = ['username', 'password1', 'password2']

class LoginForm(forms.Form):
    username = forms.CharField()
    password = forms.CharField(widget=forms.PasswordInput)


class MessageForm(forms.Form):
    message = forms.CharField(widget=forms.Textarea(attrs={'rows': 5, 'cols': 30}))
    file = forms.CharField()
    