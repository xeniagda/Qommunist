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

	TODO:	Play as government / place walls
			Window padding, extra info
			Launcher?
"""
#DISPLAY SETUP
pygame.init()
SCALE_FACTOR = 9
SCREENSIZE = 79*SCALE_FACTOR,79*SCALE_FACTOR
screen = pygame.display.set_mode(SCREENSIZE, pygame.RESIZABLE)

#SERVER
SERVER_HOST = "172.16.1.162" 
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
time.sleep(0.5)
NUM_PLAYERS = len(client.board.players)
IS_GOVERNMENT = (client.player_id >= NUM_PLAYERS)

# SPRITE FUNCTIONS
def load_image(path):
    if pygame.image.get_extended():
        return pygame.image.load(path).convert_alpha()
    if 'Image' not in locals():  # Is PIL imported?
        from PIL import Image

    im = Image.open(path)
    

    surf = pygame.image.fromstring(im.tobytes(), im.size, im.mode)

    return surf

def posPx(pos): #Return upper left px pos of tile (x,y). Tile index 1-9
	position = [SCALE_FACTOR*4,SCALE_FACTOR*4]
	position[0] += SCALE_FACTOR*8*pos[0]
	position[1] += SCALE_FACTOR*8*pos[1]
	return position

def pxPos(px): # Returns what tile a certain pixel is in.
	position = list(map(lambda x: (x-4*SCALE_FACTOR),px))
	position = list(map(lambda x: int(x/(8*SCALE_FACTOR)),position))
	return position


def offset(pos,amount): # amount == (x,y), offset in blueprint pixels
	pos[0] += amount[0]*SCALE_FACTOR
	pos[1] += amount[1]*SCALE_FACTOR
	return pos

def invertY(pos):
	pos = list(pos)
	pos[1] = 8-pos[1]
	return pos

def scalePlayers(sprites):
	sprites = list(sprites)
	for i in range(len(sprites)):
		sprites[i] = pygame.transform.scale(sprites[i],(SCALE_FACTOR*6,)*2)
	return sprites

def prepare_sprites():
	global boardSprite_raw, playerSprites_raw, wallSprite_raw, highlightSprite_raw, ghostSprite_raw

	boardSprite_raw = load_image("Assets/board1.png")
	playerSprites_raw = [load_image("Assets/pawn%s.png" % n) for n in range(4)]
	wallSprite_raw = load_image("Assets/wall1.png")
	highlightSprite_raw = load_image("Assets/highlight1.png")
	if IS_GOVERNMENT:
		ghostSprite_raw = load_image("Assets/ghost1.png")

def scale_sprites():
	global boardSprite, playerSprites, wallSprite, highlightSprite, ghostSprite
	x = SCALE_FACTOR
	boardSprite = pygame.transform.scale(boardSprite_raw,(79*x,79*x))

	playerSprites = scalePlayers(playerSprites_raw)

	wallSprite = pygame.transform.scale(wallSprite_raw,(x*15,x))

	highlightSprite = pygame.transform.scale(highlightSprite_raw,(x*7,x*7))

	if IS_GOVERNMENT:
		ghostSprite = pygame.transform.scale(ghostSprite_raw,(x*15,x))


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
	if IS_GOVERNMENT:
		if event.key == pygame.K_UP:
			ghost[1] += -1		
		if event.key == pygame.K_DOWN:
			ghost[1] += 1
		if event.key == pygame.K_LEFT:
			ghost[0] += -1
		if event.key == pygame.K_RIGHT:
			ghost[0] += 1
	else:
		if event.key == pygame.K_UP:
			client.doMove(b"gu")		
		if event.key == pygame.K_DOWN:
			client.doMove(b"gd")	
		if event.key == pygame.K_LEFT:
			client.doMove(b"gl")	
		if event.key == pygame.K_RIGHT:
			client.doMove(b"gr")

def moveTo(tile):
	if IS_GOVERNMENT:
		ghost = (tile[0],tile[1])
	else:
		direction = [0,0]
		direction[0] = tile[0]-players[client.player_id][0]
		direction[1] = tile[1]-players[client.player_id][1]

		if direction == [0,1]:
			client.doMove(b"gd")
		if direction == [1,0]:
			client.doMove(b"gr")
		if direction == [0,-1]:
			client.doMove(b"gu")
		if direction == [-1,0]:
			client.doMove(b"gl")
	
if IS_GOVERNMENT:
	ghost = [0,0,True] # Vertical?

prepare_sprites()
scale_sprites()

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

	screen.blit(boardSprite,(0,0))
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

		if event.type == pygame.KEYDOWN and turn == client.player_id:
			move(event)

		if event.type == pygame.MOUSEBUTTONUP and turn == client.player_id:
			pos = pxPos(pygame.mouse.get_pos())
			moveTo(pos)

		if (event.type == pygame.KEYDOWN and event.key == pygame.K_RETURN and turn == client.player_id and IS_GOVERNMENT):

			direction = ["u" if ghost[2] else "r"]
			data = "p{}{}{}".format(ghost[0],direction[0],8-ghost[1])
			client.doMove(bytes(data, "ascii")) 

		if (event.type == pygame.KEYDOWN and event.key == pygame.K_SPACE and turn == client.player_id and IS_GOVERNMENT):
			ghost[2] = not ghost[2]

	#Draw players
	for i in range(len(players)):
		pos = posPx(players[i])
		pos = offset(pos,(0.5,0.5)) #Adjust from top left corner.
		screen.blit(playerSprites[i],pos)

	#Draw walls
	for x,y,vertical in walls:
		pos = posPx((x,y))
		localSprite = wallSprite
		if vertical:
			localSprite = pygame.transform.rotate(localSprite,90)
			pos = offset(pos,(-1,0))
		else:
			pos = offset(pos,(-8,7))
		screen.blit(localSprite,pos)

	#Draw details
	if turn in range(NUM_PLAYERS): # Draw highlight
		pos = posPx(players[turn])
		pos = offset(pos,(0,0))
		screen.blit(highlightSprite,pos)

	if trigger_rescale:
		trigger_rescale = False
		scale_sprites()

	if IS_GOVERNMENT and turn == client.player_id:
		ghost = [max(ghost[0], 0), max(ghost[1], 0),ghost[2]] #Keeps ghost within board
		ghost = [min(ghost[0], 8), min(ghost[1], 8),ghost[2]]
		pos = posPx(ghost)
		localSprite = ghostSprite
		if ghost[2]: # Is vertical?
			localSprite = pygame.transform.rotate(localSprite,90)
			pos = offset(pos,(-1,0))
		else:
			pos = offset(pos,(-8,7))
		screen.blit(localSprite,pos)


	pygame.display.set_caption("You are player %s, it's player %s's turn" % (client.player_id+1,turn+1))


	pygame.display.flip()
	pygame.display.fill((0,0,0))
	time.sleep(0.01)

