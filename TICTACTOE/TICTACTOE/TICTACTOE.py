import numpy as np
import reflex as rx

class Gato:
    def __init__(self):
        self.board = np.zeros((3,3))

        self.HUMAN = +1
        self.CPU = -1

    def heuristic(self, state):
        if self.win(state, self.CPU):
            score = +10
        elif self.win(state, self.HUMAN):
            score = -10
        else:
            score = 0

        return score

    def win(self, state, player):
        win_state = [
            [state[0, 0], state[0, 1], state[0, 2]],
            [state[1, 0], state[1, 1], state[1, 2]],
            [state[2, 0], state[2, 1], state[2, 2]],
            [state[0, 0], state[1, 0], state[2, 0]],
            [state[0, 1], state[1, 1], state[2, 1]],
            [state[0, 2], state[1, 2], state[2, 2]],
            [state[0, 0], state[1, 1], state[2, 2]],
            [state[2, 0], state[1, 1], state[0, 2]]
        ]

        if [player, player, player] in win_state:
            return True
        else:
            return False

    def game_over(self, state):
        return self.win(state, self.HUMAN) or self.win(state, self.CPU)

    def valid_moves(self, state, x, y):
        empty_cells = np.argwhere(state == 0)
        if [x, y] in empty_cells:
            return True
        else:
            return False

    def move(self, x, y, player):
        if (self.valid_moves(self.board, x, y)):
            self.board[x, y] = player
            return True
        else:
            return False

    def minimax(self, state, depth, player):

        if player == self.CPU:
            best = [-1, -1, -np.inf]
        else:
            best = [-1, -1, +np.inf]

        if depth == 0 or self.game_over(state):
            score = self.heuristic(state)
            return [-1, -1, score]

        for cell in np.argwhere(state == 0):
            x, y = cell[0], cell[1]
            state[x, y] = player
            score = self.minimax(state, depth - 1, -player)
            state[x, y] = 0
            score[0], score[1] = x, y

            if player == self.CPU:
                if score[2] > best[2]:
                    best = score
            else:
                if score[2] < best[2]:
                    best = score

        return best

    def cpu_turn(self):
        depth = len(np.argwhere(self.board == 0))
        if depth == 0 or self.game_over(self.board):
            return

        if depth == 9:
            x = np.random.randint(0, 3)
            y = np.random.randint(0, 3)
        else:
            move = self.minimax(self.board, depth, self.CPU)
            x, y = move[0], move[1]

        self.move(x, y, self.CPU)

    def human_turn(self, x, y):
        depth = len(np.argwhere(self.board == 0))
        if depth == 0 or self.game_over(self.board):
            return

        potentialmove = self.move(x, y, self.HUMAN)
        if not potentialmove:
            print("Invalid move")
            return

def main():
    game = Gato()
    print("welcome to the game")
    print(game.board)

    while not game.game_over(game.board):
        print("Your turn")
        x, y = map(int, input("Enter the position: ").split())
        game.human_turn(x, y)
        print(game.board)
        print("CPU Move")
        game.cpu_turn()
        print(game.board)

        if game.win(game.board, game.HUMAN):
            print("Human wins!")
            break
        elif game.win(game.board, game.CPU):
            print("CPU wins!")
            break
        elif len(np.argwhere(game.board == 0)) == 0:
            print("It's a tie!")
            break

#main()

##### Use Reflex for UI ########################################################

class GameState(rx.State):

    board: list[list[str]] = [["", "", ""], ["", "", ""], ["", "", ""]]
    _game: Gato = Gato()
    message: str = "TIC TAC TOE!"
    buttonmessage: str = "Reset"

    def update_board(self):
        #self.board = self._game.board.tolist()
        for i in range(3):
            for j in range(3):
                if self._game.board[i, j] == self._game.HUMAN:
                    self.board[i][j] = "X"
                elif self._game.board[i, j] == self._game.CPU:
                    self.board[i][j] = "O"
                else:
                    self.board[i][j] = ""
    
    def handle_click(self, x, y):

        self._game.human_turn(x, y)
        self.update_board()
        self._game.cpu_turn()
        self.update_board()

        if self._game.win(self._game.board, self._game.HUMAN):
            self.message = "Human wins!"
            self.buttonmessage = "Play Again"
        elif self._game.win(self._game.board, self._game.CPU):
            self.message = "CPU wins!"
            self.buttonmessage = "Play Again"
        elif len(np.argwhere(self._game.board == 0)) == 0:
            self.message = "It's a tie!"
            self.buttonmessage = "Play Again"

    def resetGame(self):
        self._game = Gato()
        self.message = "TIC TAC TOE!"
        self.board = [["", "", ""], ["", "", ""], ["", "", ""]]
        self.buttonmessage = "Reset"

def title():
    return rx.flex(
    rx.text(
        f"{GameState.message}",
        weight="bold",
        align="center",
        as_="div",
    ),
    direction="column",
    spacing="3",
    width="100%",
    size="10",
    style={"padding-top": "5%"},
)



def index():
    return title(), rx.grid(
            rx.button(f"{GameState.board[0][0]}", on_click=GameState.handle_click(0, 0), color_scheme=rx.cond(GameState.board[0][0] == "", "gray", rx.cond(GameState.board[0][0] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[0][1]}", on_click=GameState.handle_click(0, 1), color_scheme=rx.cond(GameState.board[0][1] == "", "gray", rx.cond(GameState.board[0][1] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[0][2]}", on_click=GameState.handle_click(0, 2), color_scheme=rx.cond(GameState.board[0][2] == "", "gray", rx.cond(GameState.board[0][2] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[1][0]}", on_click=GameState.handle_click(1, 0), color_scheme=rx.cond(GameState.board[1][0] == "", "gray", rx.cond(GameState.board[1][0] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[1][1]}", on_click=GameState.handle_click(1, 1), color_scheme=rx.cond(GameState.board[1][1] == "", "gray", rx.cond(GameState.board[1][1] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[1][2]}", on_click=GameState.handle_click(1, 2), color_scheme=rx.cond(GameState.board[1][2] == "", "gray", rx.cond(GameState.board[1][2] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[2][0]}", on_click=GameState.handle_click(2, 0), color_scheme=rx.cond(GameState.board[2][0] == "", "gray", rx.cond(GameState.board[2][0] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[2][1]}", on_click=GameState.handle_click(2, 1), color_scheme=rx.cond(GameState.board[2][1] == "", "gray", rx.cond(GameState.board[2][1] == "O", "blue", "pink")),height="100%"),
            rx.button(f"{GameState.board[2][2]}", on_click=GameState.handle_click(2, 2), color_scheme=rx.cond(GameState.board[2][2] == "", "gray", rx.cond(GameState.board[2][2] == "O", "blue", "pink")),height="100%"),
            columns="3",
            spacing="1",
            width="100%",
            height="75vh",
            style={"padding": "2%"}
        ),  rx.stack(
            rx.button(f"{GameState.buttonmessage}", on_click=GameState.resetGame(), width="100%", background_image="linear-gradient(144deg,#AF40FF,#5B42F3 50%)"),
            style={"padding": "2%", "padding-right": "2%"}
        )

app = rx.App()
app.add_page(
    index
)