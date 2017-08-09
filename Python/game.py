import pygame
import numpy as np
import time

import inputBox
import Player

#DISPLAY SETUP
pygame.init()
scale_factor = 10
screensize = width, hight = 79*scale_factor,79*scale_factor
screen = pygame.display.set_mode(screensize)

#SERVER
SERVER_HOST = "172.16.1.162"   #"localhost"
PORT = 1961
ONLINE = True

NUM_PLAYERS = 2

#CONNECT TO SERVER
if ONLINE:
	client = Player.Client(SERVER_HOST, PORT)
	games = client.getGames()
	print(client)
	if not games:
		client.createBoard()
	else:
		gameId = inputBox.ask(screen,"Choose a GameID: " + str(games))
		client.connectTo(gameId)
else:
	games = [1]
	client = None

def load_image(path):
    if pygame.image.get_extended():
        return pygame.image.load(path).convert()
    if 'Image' not in locals():  # Is PIL imported?
        from PIL import Image

    im = Image.open(path)
    

    surf = pygame.image.fromstring(im.tobytes(), im.size, im.mode)

    return surf

#DRAW BOARD
board = load_image("Assets/board1.png").convert()
board = pygame.transform.scale(board,screensize)
screen.blit(board,(0,0))

def pxPos(pos): #Return upper left px pos of tile x,y. Tile index 1-9
	position = [scale_factor*4,scale_factor*4]
	position[0] += scale_factor*8*pos[0]
	position[1] += scale_factor*8*pos[1]
	return position

def scaleSprites(sprites,scale_factor): #Scale player sprites
	for sprite in sprites:
		sprite = pygame.transform.scale(sprite,(scale_factor*8,)*2)
	return sprites

#PLAYER DATA
players = {1:(4,8), 2:(4,0), 3:(0,4), 4:(8,4)} #(ID,x,y) x,y: 0-8
playerSprites = [load_image("Assets/pawn%s.png" % n).convert_alpha() for n in range(4)]
playerSprites = scaleSprites(playerSprites,scale_factor)

#WALL DATA
walls = [[1,6,True],[4,3,False]] #Pos: 1-8, Bool(Vertical)
wallSprite = load_image("Assets/wall1.png")
wallSprite = pygame.transform.scale(wallSprite,(scale_factor*15,scale_factor))

def invertY(pos):
	pos = list(pos)
	pos[1] = 8-pos[1]
	return pos

def unpackPlayers(): #Excluding the goverment
	players = {}
	playersData = client.board.players
	for i in range(len(playersData)):
		players[i] = invertY(playersData[i][1:])

	return players

def unpackWalls():
	walls = list(map(list,client.board.walls))
	walls = list(map(invertY,walls))
	return walls

time.sleep(0.5)
while True:
	players = unpackPlayers()
	walls = unpackWalls()
	for event in pygame.event.get():
		if event.type == pygame.QUIT:
			print("Quitting Qommunist")
			exit()
	#Listen to server -> Update (player) & (walls)

	for i in range(len(players)):
		pos = pxPos(players[i])
		pos = list(map(lambda x: int(x+scale_factor*0.5),pos))#Adjust from top left corner.
		screen.blit(playerSprites[i],pos)

	for x,y,vertical in walls:
		pos = pxPos((x,y))
		pos[1] = pos[1]-scale_factor#Adjust from top left corner of tile.
		wSprite = wallSprite
		if vertical:
			wSprite = pygame.transform.rotate(wSprite,90)
		screen.blit(wSprite,pos)

	pygame.display.flip() #update(players, walls)
	screen.blit(board,(0,0)) #Clear screen
	time.sleep(0.01)

