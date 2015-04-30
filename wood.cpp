#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef unsigned long long int board;

#define WIDTH 8

typedef struct state {
    board aline;
    board bline;
    board king;
} state;

typedef struct move {
    board first;
    board take;
    int score;
} move;

char *btostr(board num) {
    static char retbuf[(WIDTH+1)*6+1];
    char *p;

    p = &retbuf[sizeof(retbuf)-1];
    *p = '\0';

    for (int i=0; i<64; i++) {
        if (i < WIDTH*6) {
            if (i%WIDTH == 0)
                *--p = '\n';
            *--p = (num & 1) ? '1' : '0';
        }
        num /= 2;
    };

    return p;
}

void printb(board num) {
    printf("%s", btostr(num));
}

char *stostr(state s) {
    static char retbuf[(WIDTH+1)*6+1];
    char *p;

    p = &retbuf[sizeof(retbuf)-1];
    *p = '\0';

    for (int i=0; i<64; i++) {
        if (i < WIDTH*6) {
            if (i%WIDTH == 0)
                *--p = '\n';
            *--p = (s.aline & 1) ? 'A' : ((s.bline & 1) ? 'B' : '.');
        }
        s.aline /= 2;
        s.bline /= 2;
    };

    return p;
}

void prints(state s) {
    printf("%s", stostr(s));
}

void printm(move m) {
    printf("Move:\n");
    printb(m.first);
    printf("Choose:\n");
    printb(m.take);
    printf("Score: %d\n", m.score);
}

#define MASK(D0,D1,D2,D3,D4,D5) (D5) | ((D4) << WIDTH) | ((D3) << (WIDTH*2)) | ((D2) << (WIDTH*3)) | ((D1) << (WIDTH*4)) | ((D0) << (WIDTH*5))
#define LINES(L) MASK(L,L,L,L,L,L)

static const board cell = (1<<WIDTH)-1;

static const board amsk  = LINES(cell);
static const board trimr = LINES((1LL<<(WIDTH-1))-1);  //01111111
static const board triml = LINES((1LL<<WIDTH)-2);      //11111110
static const board oner  = LINES(1LL);                 //00000001
static const board onel  = LINES(1LL<<7);              //10000000
static const board vmsk  = LINES((1LL<<(WIDTH-1))-2);  //01111110
static const board hmsk  = MASK(0LL,cell,cell,cell,cell,0LL);
static const board htrr  = MASK(cell,cell,cell,cell,cell,0LL);
static const board htrl  = MASK(0LL,cell,cell,cell,cell,cell);

// Given line, gives bits with 1s on either side
inline board holes(board line) {
    return ((line << 1) & (line >> 1)) & vmsk;
}
inline board holesv(board line) {
    return ((line << WIDTH) & (line >> WIDTH)) & hmsk;
}

// As for holes, but include bits against edges
inline board holes_margins(board line) {
    return (((line << 1) | oner) & ((line >> 1) | onel)) & amsk;
}
inline board holes_marginsv(board line) {
    return (((line << WIDTH) | htrr) & ((line >> WIDTH) | htrl)) & amsk;
}

// Given line holes, gives bits from check surrounded by 1s
inline board vulnerable(board line, board check) {
    return line & check;
}

// Bits surrounded on at least two sides
inline board surrounded_two(board line, board check) {
    return vulnerable(holes(line), check) | vulnerable(holesv(line), check);
}
// Bits surrounded on four sides
inline board surrounded_four(board line, board check) {
    return vulnerable(holes(line), check) & vulnerable(holesv(line), check);
}
// Bits surrounded including margins
inline board surrounded_margins(board line, board check) {
    return vulnerable(holes_margins(line), check) & vulnerable(holes_marginsv(line), check);
}

// Bits on either side of a bit
inline board sides(board pos) {
    return (((pos << 1) & triml) | ((pos >> 1) & trimr)) & vmsk;
}
inline board sidesv(board pos) {
    return (((pos << WIDTH)) | ((pos >> WIDTH))) & hmsk;
}

// Is the current move surrounding something?
inline board check_current(state s, board current) {
    return surrounded_two(s.aline, s.bline) & (sides(current) | sidesv(current));
}

// Is the current move surrounding the king?
inline board check_king(state s, board current) {
    return surrounded_four(s.aline, s.bline) & (sides(current) | sidesv(current)) & s.king;
}

// Is opponent surrounded?
inline bool check_completely_surrounded(board aline, board bline) {
    return surrounded_margins(aline | bline, bline) == bline;
}

move each_position(state s, int depth, int alpha, int beta);

// A just moved, check it
// aline does not contain amove
inline move check_state(state s, board amove, int depth, int* alpha, int beta) {
    if (depth > 50) {
        printf("Depth: %d\n", depth);
        prints(s);
    }
    board line_match = check_current(s, amove);
    board king_match = check_king(s, amove);

    if (king_match) {
        return {s.aline, 0, 100};
    }

    int best = INT_MIN;
    board bestpos = 0;

    if (line_match) {
      while (line_match) {
        board pos  = line_match & (~line_match + 1);
        board newb = s.bline ^ pos;

        if (check_completely_surrounded(s.aline, newb)) {
          return {s.aline, pos, 100};
        }

        if (depth > 50)
          printf("A<->B\n");
        unsigned int ret = -each_position({newb, s.aline, s.king}, depth - 1, -beta, -*alpha).score;
        if (depth > 50)
          printf("B<->A\n");
        if (ret > best) {
          best = ret + 1;
          bestpos = pos;
        }
        *alpha = *alpha > best ? *alpha : best;
        if (*alpha >= beta)
          return {s.aline, bestpos, best};

        line_match &= line_match - 1;
      }
    } else {
        if (depth > 50)
            printf("A<->B\n");
        best = -each_position({s.bline, s.aline, s.king}, depth - 1, -beta, -*alpha).score;
        if (depth > 50)
            printf("B<->A\n");
    }

    return {s.aline, bestpos, best};
}

// Move current peice until collision
#define WALK(STEP) \
    pos = orig_pos;\
    while ( STEP ) {\
        moves++;\
        board newa = astripped ^ pos;\
        board newk = kstripped ^ pos;\
        move ret = check_state({newa, s.bline, newk}, pos, depth, &alpha, beta);\
        if (ret.score >= 100)\
            return ret;\
        best = ret.score > best.score ? ret : best;\
        alpha = alpha > best.score ? alpha : best.score;\
        if (alpha >= beta)\
          return ret;\
    }\


// Iterate possible positions for a
move each_position(state s, int depth, int alpha, int beta) {
    if (depth == 0)
        return {0,0,0};
    
    board walk = s.aline;
    board line = s.aline | s.bline;
    move best = {0,0,INT_MIN};
    int moves = 0;
    while (walk) {
        board orig_pos = walk & (~walk + 1);
        board pos = orig_pos;
        
        board astripped = s.aline ^ orig_pos;
        board kstripped = s.king  ^ orig_pos;

        WALK(((pos >>= 1) && !(pos & (line | onel)))); //right
        WALK(!((pos <<= 1) & (line | oner | ~amsk)));  //left
        WALK(((pos >>= WIDTH) && !(pos & line)));      //down
        WALK(!((pos <<= WIDTH) & (line | ~amsk)));     //up
        
        walk &= walk - 1;
    }
    
    if (depth > 3) {
        printf("After %d I found:\n", depth);
        prints(s);
        printm(best);
        printf("I made %d moves\n", moves);
    }
    return best;
}



int main() {
    board linea = MASK(cell, 0x10LL, 0LL, 0LL, 0LL, 0LL);
    //board lineb = MASK(0LL, 0LL, 0LL, 0LL, 0x8LL, cell);
    board lineb = MASK(0LL, 0x20LL, 0LL, 0LL, 0X8LL, cell - 1);
    board kings = MASK(0LL, 0x10LL, 0LL, 0LL, 0x8LL, 0LL);
    state s = {linea, lineb, kings};
    prints(s);
    printf("-----\n");
    move m = each_position(s, 10, INT_MIN, INT_MAX);
    printm(m);
    return 0;
}
