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

	TODO: Play as government
		  Rescale sprites, extra information
"""
#DISPLAY SETUP
pygame.init()
SCALE_FACTOR = 10
SCREENSIZE = 79*SCALE_FACTOR,79*SCALE_FACTOR
screen = pygame.display.set_mode(SCREENSIZE, pygame.RESIZABLE)

#SERVER
SERVER_HOST = "localhost" #"172.16.1.162" 
PORT = 1961
ONLINE = True

#CONNECT TO SERVER
if ONLINE:
	client = Player.Client(SERVER_HOST, PORT)
	games = client.getGames()
	print(client)
	gameID = inputBox.ask(screen,"Choose a GameID: " + str(games))
	try:
		if int(gameID) in games:
			client.connectTo(gameID)
		else:
			client.createBoard()
	except: client.createBoard()

else:
	games = [1]
	client = None

NUM_PLAYERS = len(client.board.players)
PLAYER_ID = client.player_id
IS_GOVERNMENT = (PLAYER_ID > NUM_PLAYERS)

# SPRITE FUNCTIONS
def load_image(path):
    if pygame.image.get_extended():
        return pygame.image.load(path).convert_alpha()
    if 'Image' not in locals():  # Is PIL imported?
        from PIL import Image

    im = Image.open(path)
    

    surf = pygame.image.fromstring(im.tobytes(), im.size, im.mode)

    return surf

def pxPos(pos): #Return upper left px pos of tile (x,y). Tile index 1-9
	position = [SCALE_FACTOR*4,SCALE_FACTOR*4]
	position[0] += SCALE_FACTOR*8*pos[0]
	position[1] += SCALE_FACTOR*8*pos[1]
	return position

def offset(pos,amount): # amount == (x,y), offset in blueprint pixels
	pos[0] += amount[0]*SCALE_FACTOR
	pos[1] += amount[1]*SCALE_FACTOR
	return pos

def invertY(pos):
	pos = list(pos)
	pos[1] = 8-pos[1]
	return pos

def scaleSprites(sprites): #Scale player sprites
	for sprite in sprites:
		sprite = pygame.transform.scale(sprite,(SCALE_FACTOR*8,)*2)
	return sprites

def prepare_sprites():
	global boardSprite_raw, playerSprites_raw, wallSprite_raw, highlightSprite_raw

	boardSprite_raw = load_image("Assets/board1.png")
	playerSprites_raw = [load_image("Assets/pawn%s.png" % n) for n in range(4)]
	wallSprite_raw = load_image("Assets/wall1.png")
	highlightSprite_raw = load_image("Assets/highlight1.png")

def scale_sprites():
	global boardSprite, playerSprites, wallSprite, highlightSprite
	boardSprite = pygame.transform.scale(boardSprite_raw,SCREENSIZE)

	playerSprites = scaleSprites(playerSprites_raw,SCALE_FACTOR)

	wallSprite = pygame.transform.scale(wallSprite_raw,(SCALE_FACTOR*15,SCALE_FACTOR))

	highlightSprite = pygame.transform.scale(highlightSprite_raw,(SCALE_FACTOR*8,SCALE_FACTOR*8))


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

def move(event):
	if event.key == pygame.K_UP:
		client.doMove(b"gu")		
	if event.key == pygame.K_DOWN:
		client.doMove(b"gd")	
	if event.key == pygame.K_LEFT:
		client.doMove(b"gl")	
	if event.key == pygame.K_RIGHT:
		client.doMove(b"gr")

#while True:
#	if client.board.started():
#		break
#	time.sleep(0.5)

trigger_rescale = False
while True:
	#Listen to server -> Update
	players = unpackPlayers()
	walls = unpackWalls()
	turn = client.board.turn # Whos turn?

	#Listen to events
	for event in pygame.event.get():
		if event.type == pygame.QUIT:
			print("Quitting Qommunist")
			exit()
		if event.type == pygame.VIDEORESIZE:
			SCREENSIZE = event.dict['size']
			SCALE_FACTOR=int(min(SCREENSIZE)/79)
			screen = pygame.display.set_mode(SCREENSIZE, pygame.RESIZABLE)
			trigger_rescale = True

		if event.type == pygame.KEYDOWN:
			move(event)

	#Draw players
	for i in range(len(players)):
		pos = pxPos(players[i])
		pos = offset(pos,(0.5,0.5)) #Adjust from top left corner.
		screen.blit(playerSprites[i],pos)

	#Draw walls
	for x,y,vertical in walls:
		pos = pxPos((x,y))
		wSprite = wallSprite
		if vertical:
			wSprite = pygame.transform.rotate(wSprite,90)
			pos = offset(pos,(-1,0))
		else:
			pos = offset(pos,(-8,7))
		screen.blit(wSprite,pos)

	#Draw details
	if turn in range(NUM_PLAYERS): # Draw highlight
		pos = pxPos(players[turn])
		pos = offset(pos,(0,0))
		screen.blit(highlightSprite,pos)

	if trigger_rescale:
		trigger_rescale = False
		scale_sprites()

	if IS_GOVERNMENT:
		pass

	pygame.display.flip()
	screen.blit(board,(0,0))
	time.sleep(0.01)

