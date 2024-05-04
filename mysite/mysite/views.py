
from django.shortcuts import render, redirect
from django.contrib.auth import authenticate, login, logout 
from .forms import SignupForm, LoginForm, MessageForm
from django.http import HttpResponse
import requests as rqsts
import json
import os
import random
import subprocess

ROOT_DIR = "/home/sv/Documents/Sandeep/CS613/Project2"

def get_list(user_id, file):
    with open(f"{ROOT_DIR}/tmp_{user_id}/{file}", "r") as file:
        lines = file.read()

    lines = list(lines.split("\n"))
    lines = [req for req in lines if req]

    return lines


######################## AUTH AND INITIALIZATION ############################

    
def get_free_port():
     while True:
        port = random.randint(7000, 9000)
        if os.system(f"lsof -i:{port}") != 0:
            return port


def init_client(request, username):
    api_port = get_free_port()
    socket_port = get_free_port()
    request.session['user_id'] = username
    request.session['api_port'] = api_port
    request.session['socket_port'] = socket_port


    ##store the details in a json file
    CONFIG_FILE = ROOT_DIR + "/" + username + ".json"
    with open(CONFIG_FILE, "w") as file:
        json.dump({'apiport': api_port, 'port': socket_port, 'userid' : username, 'ip' : "0.0.0.0"}, file)

    TMP_DIR = ROOT_DIR + "/tmp_" + username
    if not os.path.exists(TMP_DIR):
        os.makedirs(TMP_DIR)

    REQ_FILE = TMP_DIR + "/requests.txt"
    with open(REQ_FILE, "w") as file:
        file.write("")

    PLAY_REQ_FILE = TMP_DIR + "/play.txt"
    with open(PLAY_REQ_FILE, "w") as file:
        file.write("")


    LOG_FILE = TMP_DIR + "/log.txt"
    with open(LOG_FILE, "w") as file:
        file.write("")
    
    subprocess.Popen(["cabal", "run", "client", "--", f"{username}.json"], cwd=f"{ROOT_DIR}/client", stdout=open(LOG_FILE, "w"), stderr=open(LOG_FILE, "w"))

# signup page
def user_signup(request):
    if request.user.is_authenticated:
        return redirect('home')
    else: 
        if request.method == 'POST':
            form = SignupForm(request.POST)
            if form.is_valid():
                form.save()
                return redirect('login')
            else:
                form = SignupForm()
                args = { 'form': form , 'success' : False }
                return render(request,'signup.html',args)
        else:
            form = SignupForm()
            args = {'form': form , 'success' : True }
            return render(request,'signup.html',args)

# login page
def user_login(request):
    if request.user.is_authenticated:
        return redirect('home')

    if request.method == 'POST':
        form = LoginForm(request.POST)
        if form.is_valid():
            username = form.cleaned_data['username']
            password = form.cleaned_data['password']
            user = authenticate(request, username=username, password=password)
            if user:
                login(request, user)
                init_client(request, username)    
                return redirect('home')
    else:
        form = LoginForm()
    return render(request, 'login.html', {'form': form})

# logout page
def user_logout(request):
    if request.user.is_authenticated:
        logout(request)
    return redirect('login')


###########################################################################################

def home(request):
    if not request.user.is_authenticated:
        return redirect('login')
    
    user_id = request.session.get('user_id')
    
    requests = get_list(user_id, "requests.txt")
    play_requests = get_list(user_id, "play.txt")

    # score = get_info(user_id)
    score = 0

    return render(request, 'home.html', { 'users_chat' : requests, 'users_play' : play_requests, 'score' : score})


def chat_home(request):
    if not request.user.is_authenticated:
        return redirect('login')

    if request.method == 'POST':
        user = request.POST.get('user')
        myid = request.session.get('user_id')
        api_port = request.session.get('api_port')
        url = f"http://0.0.0.0:{api_port}/connect"
        params = {'user' : user}
        response = rqsts.post(url,data=params)
        print(response.text)
        return redirect('chat', user_id=user)
    else:
        user_id = request.session.get('user_id')
        
        requests = get_list(user_id, "requests.txt")
        play_requests = get_list(user_id, "play.txt")

        return render(request, 'chathome.html', { 'users_chat' : requests, 'users_play' : play_requests})

def play_home(request):
    if not request.user.is_authenticated:
        return redirect('login')
    
    myid = request.session.get('user_id')
    err_msg = ""
    
    if request.method == 'POST':
        ## need to send a request to central server to make a matching and give the username to client
        url = "http://0.0.0.0:3500/game"
        params = {'user' : myid}
        response = rqsts.post(url,data=params)
        json_resp = json.loads(response.text)
        print(json_resp)
        if json_resp['status'] == "error":
            err_msg = "No match found. Please try again later."
        elif json_resp['status'] == "ok":
            user2 = json_resp['user']

            ## randomly choose who goes first
            choice = random.randint(0,1)
            
            ## NOW SEND A COMMAND TO CLIENT TO SEND A GAME REQUEST TO user2
            api_port = request.session.get('api_port')
            url = f"http://0.0.0.0:{api_port}/play"
            params = {'user' : user2, 'choice' : choice}
            response = rqsts.post(url,data=params)
            print(response.text)

            return redirect('play', user_id=user2)

    requests = get_list(myid, "requests.txt")
    play_requests = get_list(myid, "play.txt")

    # score = get_info(myid)
    score = 0

    return render(request, 'playhome.html', { 'users_chat' : requests, 'users_play' : play_requests, 'err_msg' : err_msg, 'score' : score})

def search(request):
    return HttpResponse("Search")


def chat(request, user_id):
    if not request.user.is_authenticated:
        return redirect('login')
    
    ## if the chat file doesnt exists then just send a error message
    myid = request.session.get('user_id')
    chat_file = f"{ROOT_DIR}/tmp_{myid}/{user_id}.txt"
    if not os.path.exists(chat_file):
        f = open(chat_file, "w")
        f.close()

    if request.method == 'POST':
        message = request.POST.get('message')
        filepath = request.POST.get('filepath')

        with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/{user_id}.txt", "a") as file:
            if(message != "" and message != None):
                file.write("SEND_" + message + "\n")
            if(filepath != "" and filepath != None):
                file.write("FILE_" + filepath + "\n")

        return redirect('chat', user_id=user_id)

    messages = get_list(myid, user_id + ".txt")
    tagged_messages = []

    for msg in messages:
        if(msg.startswith("SEND_")):
            tagged_messages.append(('send', msg[5:]))
        elif(msg.startswith("GET_")):
            tagged_messages.append(('get', msg[4:]))
        elif(msg.startswith("FILE_")):
            tagged_messages.append(('file', msg[5:]))
        elif msg.startswith("FILEGET_"):
            file_name = msg[8:]
            file_browser_path = ROOT_DIR + "/tmp_" + myid + "/" + file_name
            tagged_messages.append(('fileget', file_name, file_browser_path))

    msg_form = MessageForm()

    return render(request, 'chat.html', {'myid' : myid, 'user2' : user_id, 'msgs' : tagged_messages, 'form' : msg_form})



############################### PLAY RELATED UTILS ####################################

def key_validate(key):
    if len(key) != 3:
        return False
    if key[0] == key[1] or key[1] == key[2] or key[0] == key[2]:
        return False
    return True

def get_key_word(prefix, messages, round = 0):
    flipdone = 0
    for msg in messages:
        if round == 1 and flipdone == 0:
            if msg.startswith("FLIP"):
                flipdone = 1
            continue

        if msg.startswith(prefix):
            if key_validate(msg[len(prefix):]):
                return msg[len(prefix):]
    return ""

def get_cows_bulls(key, guess):
    bulls = 0
    cows = 0
    for i in range(3):
        if key[i] == guess[i]:
            bulls += 1
        elif guess[i] in key:
            cows += 1
    return bulls, cows

def process_guesses(prefix, messages, key, round = 0):
    guesses = []
    counter = 1
    done = 0
    flipdone = 0
    for msg in messages:
        if round == 0 and msg.startswith("FLIP"):
            break
        if round == 1 and flipdone == 0:
            if msg.startswith("FLIP"):
                flipdone = 1
            continue
        guess = {}
        if msg.startswith(prefix):
            guess['SN'] = counter
            guess['guess'] = msg[len(prefix):]
            if key_validate(guess['guess']):
                guess['valid'] = 1
                bulls, cows = get_cows_bulls(key, guess['guess'])
                if(bulls == 3):
                    done = 1
                guess['bulls'] = bulls
                guess['cows'] = cows
                counter += 1
            else:
                guess['valid'] = 0
            guesses.append(guess)

    return guesses, done

def get_round(messages):
    for msg in messages:
        if msg.startswith("FLIP"):
            return 1
    return 0

def check_for_restart(messages):
    for msg in reversed(messages):
        if msg.startswith("GET_RESTART"):
            return msg[11:]
    return ""

def play(request, user_id):
    if not request.user.is_authenticated:
        return redirect('login')
    
    if request.method == 'POST':
        key = request.POST.get('key')
        guess = request.POST.get('guess')

        if(key != "" and key != None):
            with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/game_{user_id}.txt", "a") as file:
                file.write("SEND_" + key + "\n")

        if(guess != "" and guess != None):
            with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/game_{user_id}.txt", "a") as file:
                file.write("SEND_" + guess + "\n")

    messages = get_list(request.session.get('user_id'), "game_" + user_id + ".txt")


    # str = check_for_restart(messages)
    # if str != "":
    #     with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/game_{user_id}.txt", "w") as file:
    #         file.write("PLAY" + str + "\n")
    #     return redirect('play', user_id=user_id)


    game1 = {'done' : 0, 'guesses' : []}
    game2 = {'done' : 0, 'guesses' : []}
    round = get_round(messages)
    starter = 0
    stop = 0

    if messages[0] == "PLAY0":
        game1['guesser'] = 1
        game2['guesser'] = 0
        starter = 0
    else:
        game1['guesser'] = 0
        game2['guesser'] = 1
        starter = 1



    ######### ROUND 1 #########
    game1['key'] = get_key_word( "SEND_" if starter else "GET_" , messages, 0)
    if game1['key'] != "":
        game1['guesses'], done = process_guesses( "GET_" if starter else "SEND_", messages, game1['key'], 0)
    
        if done == 1 and round == 0:
            game1['done'] = 1
            with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/game_{user_id}.txt", "a") as file:
                file.write("FLIP\n")

            round = 1
    if round == 1:
        game2['key'] = get_key_word( "GET_" if starter else "SEND_" , messages, 1)
        if game2['key'] != "":
            game2['guesses'], done = process_guesses( "SEND_" if starter else "GET_", messages, game2['key'], 1)

            if done == 1:
                game2['done'] = 1
                stop = 1

                url = f"http://0.0.0.0:3500/info"
                params = {'user1' : request.session.get('user_id'), 'user2' : user_id, 'score1' : len(game1['guesses']), 'score2' : len(game2['guesses'])}
                response = rqsts.post(url,data=params)



    print(request.session.get('user_id'), game1, game2, round)

    return render(request, 'play.html', {'user2' : user_id, 'score1' : len(game1['guesses']), 'score2' : len(game2['guesses']),  'round' : round, 'stop' : stop,  'game1' : game1, 'game2' : game2})

def restart(request, user_id):
    if not request.user.is_authenticated:
        return redirect('login')

    # start = random.randint(0,1)
    # with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/game_{user_id}.txt", "a") as file:
    #     file.write("SEND_RESTART" + str(start) + "\n")

    # # sleep for 2 seconds to let the client send message
    # os.system("sleep 2")

    # with open(f"{ROOT_DIR}/tmp_{request.session.get('user_id')}/game_{user_id}.txt", "w") as file:
    #     file.write("PLAY" + str(1 - start) + "\n")

    return redirect('play', user_id=user_id)

def store_info(user1, user2, score1, score2 ):
    url = f"http://0.0.0.0:3500/info"
    params = {'user1' : user1, 'user2' : user2, 'score1' : score1, 'score2' : score2}
    response = rqsts.post(url,data=params)

def get_info(user):
    url = f"http://0.0.0.0:3500/getinfo"
    params = {'user' : user}
    response = rqsts.post(url,data=params)
    response_json = json.loads(response.text)
    return response_json['score']

def catch_all(request):
    return redirect('home')