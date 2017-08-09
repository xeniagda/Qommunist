import pygame
import numpy as np
import time

import inputBox
import Player
"""
1. Setup Display
2. Connect to server
3. Define sprite functions
4. Load & Scale Sprites
5. Functions for unpacking server data
Game Loop:
	1. Server
	2. Events
	3. Players
	4. Walls
	5. Reset

	TODO: Inputs
"""
#DISPLAY SETUP
pygame.init()
scale_factor = 10
screensize = width, hight = 79*scale_factor,79*scale_factor
screen = pygame.display.set_mode(screensize)

#SERVER
SERVER_HOST = "172.16.1.162"   #"localhost"
PORT = 1961
ONLINE = True

#CONNECT TO SERVER
if ONLINE:
	client = Player.Client(SERVER_HOST, PORT)
	games = client.getGames()
	print(client)
	gameID = inputBox.ask(screen,"Choose a GameID: " + str(games))
	if gameID in games:
		client.connectTo(gameID)
	else:
		client.createBoard()

else:
	games = [1]
	client = None


NUM_PLAYERS = 2
IS_GOVERNMENT = (client.player_id > NUM_PLAYERS)

# SPRITE FUNCTIONS
def pxPos(pos): #Return upper left px pos of tile x,y. Tile index 1-9
	position = [scale_factor*4,scale_factor*4]
	position[0] += scale_factor*8*pos[0]
	position[1] += scale_factor*8*pos[1]
	return position

def scaleSprites(sprites,scale_factor): #Scale player sprites
	for sprite in sprites:
		sprite = pygame.transform.scale(sprite,(scale_factor*8,)*2)
	return sprites

def invertY(pos):
	pos = list(pos)
	pos[1] = 8-pos[1]
	return pos

# PREPARE SPRITES
board = pygame.image.load("Assets/board1.png").convert()
board = pygame.transform.scale(board,screensize)
screen.blit(board,(0,0))

players = {1:(4,8), 2:(4,0), 3:(0,4), 4:(8,4)}  # ID:(x,y) x,y: 0-8
playerSprites = [pygame.image.load("Assets/pawn%s.png" % n) for n in range(4)]
playerSprites = scaleSprites(playerSprites,scale_factor)

walls = [[1,6,True],[4,3,False]] #Pos: 1-8, Bool(Vertical)
wallSprite = pygame.image.load("Assets/wall1.png")
wallSprite = pygame.transform.scale(wallSprite,(scale_factor*15,scale_factor))


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
	#Listen to server -> Update (player) & (walls)
	players = unpackPlayers()
	walls = unpackWalls()

	#Listen to events
	for event in pygame.event.get():
		if event.type == pygame.QUIT:
			print("Quitting Qommunist")
			exit()

	#Draw players
	for i in range(len(players)):
		pos = pxPos(players[i])
		pos = list(map(lambda x: int(x+scale_factor*0.5),pos))#Adjust from top left corner.
		screen.blit(playerSprites[i],pos)

	#Draw walls
	for x,y,vertical in walls:
		pos = pxPos((x,y))
		pos[1] = pos[1]-scale_factor #Adjust from top left corner of tile.
		wSprite = wallSprite
		if vertical:
			wSprite = pygame.transform.rotate(wSprite,90)
		screen.blit(wSprite,pos)

	if IS_GOVERNMENT:
		pass

	pygame.display.flip() #update(players, walls)
	screen.blit(board,(0,0)) #Clear screen
	time.sleep(0.01)

