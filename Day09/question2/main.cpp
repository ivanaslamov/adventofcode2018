#include <iostream>
#include <vector>

using namespace std;

struct Node {
   long data;
   struct Node *prev;
   struct Node *next;
};

struct Node* head = NULL;

long keep() {
  struct Node* prev = head->prev;
  struct Node* next = head->next;

  prev->next = next;
  next->prev = prev;

  long data = head->data;

  free(head);

  head = prev;

  return data;
}

void advance(int steps = 1) {
  for(int i=0; i<steps && head != NULL; i++) {
    head = head->next;
  }
}

void retreat(int steps = 1) {
  for(int i=0; i<steps && head != NULL; i++) {
    head = head->prev;
  }
}

long insert(int i) {
  if (i % 23 == 0) {
    retreat(7);
    long data = keep();
    advance();

    return (i + data);
  }

  advance();

  struct Node* marble = (struct Node*) malloc(sizeof(struct Node));
  marble->data = i;
  marble->prev = head;
  marble->next = head->next;
  marble->next->prev = marble;

  head->next = marble;

  head = marble;

  return 0;
}

int main() {
  // initialized
  struct Node* root = (struct Node*) malloc(sizeof(struct Node));
  root->data = 0;

  struct Node* first = (struct Node*) malloc(sizeof(struct Node));
  first->data = 1;
  first->prev = root;
  first->next = root;

  root->prev = first;
  root->next = first;

  head = first;

  long number_of_players = 416;
  long number_of_marbles = 7161700;
  long players[416];

  for(int i=0; i<number_of_players; i++) {
    players[i] = 0;
  }

  // game
  long marble = 2;
  long player = 1;

  while(marble <= number_of_marbles) {
    long points = insert(marble);

    players[player] += points;

    marble++;
    player = (player + 1) % number_of_players;
  }

  long maximum = 0;
  for(int i=0; i<number_of_players; i++) {
    std::cout << " player " << i << " " << players[i] << std::endl;

    if( players[i] > maximum)
      maximum = players[i];
  }

  std::cout << " maximum " << maximum << std::endl;

  return 0;
}
