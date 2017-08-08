import socket
import threading
import time

VERBOSE_NET = True

class Board:
    def __init__(self, size, walls, players, turn, b_id):
        self.size = size        # Int
        self.walls = walls      # [(x, y, Horz/Vert)]
        self.players = players  # [(connected, x, y)]
        self.govern_connected = False
        self.govern_bricks_left = 0
        self.turn = turn        # Int
        self.b_id = b_id        # Int

    def started(self):
        return all(map(lambda pl: pl[0], self.players)) and self.govern_connected and self.players != []

    def __str__(self):
        return "Board(size=%s, walls=%s, players=%s, govern=%s, turn=%s, b_id=%s)" % (
                self.size,
                self.walls,
                self.players,
                self.govern_bricks_left if self.govern_connected else "nope",
                self.turn,
                self.b_id
        )

class LineIterator:
    def __init__(self):
        self.backstock = []

    def __iter__(self):
        return self
    
    def __next__(self):
        if self.backstock:
            last, self.backstock = self.backstock[0], self.backstock[1:]
            return last
        raise StopIteration

    def hasNext(self):
        return self.backstock != []

    def send(self, item):
        self.backstock.append(item)

    def wait(self):
        while not self.backstock:
            pass
        return next(self)

class Client:
    def __init__(self, server_host, port):
        self.socket = socket.socket(family=socket.AF_INET, type=socket.SOCK_STREAM)
        self.socket.connect((server_host, port))

        self.board = Board(8, [], [], 0, -1)
        self.player_id = -1

        self.rec_lines = LineIterator()

        self.reader = threading.Thread(target=self.readSocket, daemon=True)
        self.reader.start()

        self.updator = threading.Thread(target=self.update, daemon=True)

    def readSocket(self):
        read = ""
        while True:
            read += self.socket.recv(1000).decode("ascii")
            while "\n" in read:
                line_length = read.find("\n")
                line = read[:line_length]
                if line != "":
                    print(">", line)
                    self.rec_lines.send(line)
                read = read[line_length+1:]


    def getGames(self):
        games = self.rec_lines.wait()
        if games[0] == "g" and len(games) > 1:
            return list(map(int, games[1:].split(",")))
        return []

    def connectTo(self, game):
        self.socket.send(bytes(str(game) + "\n", "ascii"))
        self.player_id = int(self.rec_lines.wait())
        print("Joined game!")
        self.updator.start()
        time.sleep(0.01)

    def createBoard(self):
        self.socket.send(bytes("_", "ascii"))
        self.player_id = int(self.rec_lines.wait())
        print("Joined game!")
        self.updator.start()
        time.sleep(0.01)

    def update(self):
        while True:
            if self.rec_lines.hasNext():
                self.readBoard()
            time.sleep(0.01)

    def readBoard(self):

        board_encoded = self.rec_lines.wait()

        [size, walls, players, turn, b_id] = board_encoded.split(";")

        self.board.size = int(size)
        self.board.turn = int(turn)
        self.board.b_id = int(b_id)

        self.board.walls = []
        for wall in walls.split(","):
            (x, y, direction) = wall.split(" ")
            self.board.walls.append((int(x), int(y), direction=="u"))

        self.board.players = []
        for player in players.split(","):
            things = player.split(" ")
            connected = things[0] == "n"

            if things[1] == "g":  # Is government
                self.board.govern_connected = connected
                self.board.govern_bricks_left = int(things[2])
            else:
                self.board.players.append(
                    (
                        connected,
                        int(things[2]),
                        int(things[3]),
                    )
                )

    def __str__(self):
        return "Player(id=%s, board=%s)" % (self.player_id, self.board)

    def myTurn(self):
        return self.board.started() and self.board.turn == self.player_id

    def doMove(self, move):
        self.socket.send(move)


if __name__ == "__main__":
    SERVER_HOST = "localhost"
    PORT = 1961

    client = Client(SERVER_HOST, PORT)

    games = client.getGames()
    print("Games:", games)

    game = input("> ")
    
    try:
        val = int(game)
        if val in game:
            client.connectTo(val)
        else:
            client.createBoard()
    except ValueError:
        client.createBoard()

    while True:
        print(client)
        if client.myTurn():
            print("My turn!")
            todo = input("What to do? ")
            client.doMove(todo.encode("ascii"))
        time.sleep(1)