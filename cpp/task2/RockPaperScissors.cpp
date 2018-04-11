#include <iostream>
#include <ctime>
#include <cstdlib>
#include <string>

#define ROCK 1
#define PAPER 2
#define SCISSOR 3

#define RAND_RANGE 3

using namespace std;

/*
 * Determines if choice1 beats choice2.
 *
 * Valid values for both parameters are:
 * 1: Rock
 * 2: Paper
 * 3: Scissors
 *
 * Rock beats Scissors, Paper beats Rock, Scissors beat Paper.
 *
 * Returns true if choice1 beats choice2
 */
bool beats(int choice1, int choice2) {
    switch(choice1) {
      case ROCK: return (choice2 == SCISSOR) ?  true : false;
      case PAPER: return (choice2 == ROCK) ? true : false;
      case SCISSOR: return (choice2 == PAPER) ? true : false;
      default: return false;
    }
}


/*
 * Performs a game of Rock-Paper-Scissors.
 *
 * Prompts the player for a choice of Rock, Paper or Scissors (1, 2 or 3)
 * and chooses a second choice to play against.
 *
 * Displays the choice of the player and computer.
 *
 * Returns true if the player wins, false if the player loses.
 * Repeats the game in case of a draw.
 */

int get_computer_choice() {
  return (rand() % RAND_RANGE) + 1;
}

bool play() {
    string choices[] = {"Rock", "Paper", "Scissor"};
    int player_choice = ROCK;
    int computer_choice = ROCK;
    int player_input;
    while (player_choice == computer_choice) {
      cout << "Choose your Weapon:\n" 
           << "1: Rock\n" 
           << "2: Paper\n" 
           << "3: Scissors" 
           << endl;
      cout << "Your choice:";
      cin >> player_input;
      player_choice = max(ROCK, min(player_input, SCISSOR));
      computer_choice = get_computer_choice();

      cout << "Player: " 
           << choices[player_choice - 1] 
           << " | Computer: " 
           << choices[computer_choice - 1] << endl;     
      
      if (computer_choice == player_choice) cout << "It's a draw!" << endl;
    }
    return beats(player_choice, computer_choice);
}


/*
 * Performs three games of Rock-Paper-Scissors and returns true if the player
 * wins at least two of them.
 */
bool bestOf3() {
  int player_wins = 0;
  bool player_win;
  for (int i = 0; i < 3; i++) {
    player_win = play();
    player_wins += (player_win) ? 1 : 0;
    cout << player_wins << "/" << 3 << " Wins" << endl;
  }
  return player_wins > 1;
}


/*
 * Performs five games of Rock-Paper-Scissors and returns true if the player
 * wins at least three of them.
 */
bool bestOf5() {
  int player_wins = 0;
  bool player_win;
  for (int i = 0; i < 5; i++) {
    player_win = play();
    player_wins += (player_win) ? 1 : 0;
    cout << player_wins << "/" << 5 << " Wins" << endl;
  }
  return player_wins > 2;
}


/******************************************************************************/
// Template code starts here. Please do not edit beyond this point.
/******************************************************************************/

/*
 * Converts a game mode choice into the game mode's name for display purposes.
 *
 * Note: we can only return a raw pointer here because string literals have
 * static storage duration i.e. they survive for the whole run time of the
 * program. Returning a pointer to something that is not a string literal
 * would be dangerous!
 */
const char* gameModeToString(int mode) {
    switch(mode) {
        case 1:
            return "Single";
        case 2:
            return "Best of Three";
        case 3:
            return "Best of Five";
        default:
            return "invalid";
    }
}


int main() {
    // Initialize random seed
    srand(time(nullptr));

    int mode = 0;
    while(true) {
        cout << "Choose a game mode: \n"
             << "1: " << gameModeToString(1) << "\n"
             << "2: " << gameModeToString(2) << "\n"
             << "3: " << gameModeToString(3)  << "\n"
             << "0: exit\n";
        cout << "Your choice: ";
        cin >> mode;
        if(mode == 0) return 0;
        if(mode < 1 || mode > 3) {
            cout << "Invalid choice\n";
            continue;
        }
        break;
    }

    bool won = false;
    switch(mode) {
        case 1:
            won = play();
            break;
        case 2:
            won = bestOf3();
            break;
        case 3:
            won = bestOf5();
            break;
        default:
            // unreachable code
            return -1;
    }

    if(won) {
        cout << "Congratulations! You win the game!\n";
    }
    else {
        cout << "Unfortunate! You lose the game.\n";
    }

    return 0;
}
