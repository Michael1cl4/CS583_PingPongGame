# CS583_Final Project_Ping_Pong_Game
- Yinchao Zhu, zhuyin@oregonstate.edu
- Haoyuan Qiu, qiuha@oregonstate.edu
- Shukan Nieh, niehsh@oregonstate.edu

## Index
- [A brief description](#a-brief-description)
- [Instructions for running](#instructions-for-running)
- [Notes](#notes)
- [Milestone #1](#milestone--1)
- [Milestone #2](#milestone--2)
- [Final](#final)

## A brief description
Our project goal is to develop a ping-pong game using Haskell. At this point, the functionalities of the game can run correctly, including:
1. Movement of the bat, the ball, and the ball's rebound.
2. Switching different scenes from Instruction, Play, and End. 
3. Scoring board establishment.
4. Separate the global variables, handle keys, and main file.
5. The difficulty mechanism:
    - The ball will speed up automatically when it hits the bat to bring a more exciting late game experience. 
    - Players can tune their bat's length to take more challenges.
6. Two playing mode construction:
    - Player-to-AI battle mode.
    - Player-to-player battle mode.


## Instructions for running
1. stack is needed for our project. You can visit the following website to install.
   https://docs.haskellstack.org/en/stable/install_and_upgrade/
   Windows users can click “Windows 64-bit Installer” to install manually.
2. Once you have successfully installed it, you can use your terminal cd to the root directory of our project and type “stack build” to compile.
3. After that, you can type “stack run” to enjoy our PingPong Game.

## Notes
Some of you may encounter a missing file / unknown error when running “stack build”.
One possible solution is:
Copy the glut32.dll we provided in the root directory to /root/.stack-work/install/(some magic words)/bin, along with the cs583-exe.exe
This makes sense when we test :)


## Milestone #1
1. When we were trying to implement the difficulty into our program, we met a problem that the speed change may cause the function of bat collision to fail. 
   We found the reason is that the position of the ball may not equal to the bat position. Therefore, our collision function won't consider the ball touching the bat.
2. Because it’s the first version of the program, we are trying to make sure the scheme of the game is workable. For example, defining the game state, changing the scene of the game, creating the counting board, controlling bats’ position, and moving the ball. However, many logical functions have similar types, such as the judgement of the bat collision, the judgement of the wall collision, and the judgement of the winner. It’s better to define an extendible class to make the whole program look more structural.
3. Using artificial intelligence to control one of the bats would be a challenge for us because there are still some mechanisms and modifications of the recent functions that need to be done.


## Milestone #2
1. We implemented the Gloss module to draw the graph and handle the key reaction. After researching some information, Gloss is the best choice because it is based on OpenGL but it is easy to use and can generate cool graphics in a short time.
2. When we update the game state for each scene, we use the function composition idea to combine the different functions for calculating the position, velocity and win/lose.
3. We are still under construction to build a self-designed game state monad which applies to some functions like movement, collision, and out-of-bound. We would appreciate it if anyone can give us more suggestions.

## Final
1. What external libraries (if any) does your project depend on?
    - Our project is based on the Gloss library and refer to the tutorial website: https://andrew.gibiansky.com/blog/haskell/haskell-gloss/.
    - However, this project still contains many self designed parts, such as "changing scene", "AI control bat", "changing difficulty", "handle key", and so on.
2. What module should be loaded into GHCi? (Or, what command to run to compile and execute your project.)
    - Please refer to the section of "Instructions for running" for details, all commands are about the "stack" command in haskell.
    - Make sure the stack is installed
    - Move to the directory of the "CS583_FinalProject"
    - Run command "stack build"
    - Run command "stack run"
3. (Important!) What are some example expressions to evaluate in GHCi, or some example input sequences, that show off the important aspects of your project?
    - Because this project is to develop a game, the evaluation method is to play this game. The following is the user manual for this game, and there are three different scenes in this game.
    - In the instruction scene  
      - You can use 'M' to switch game modes between player-to-player and player-to-AI. It means that player1 could be controlled by either player or AI.
      - Players can change the difficulty by tuning the length of the bat within 5 different lengths. 
      - For example, Player 1(the right bat) can adjust their bet length with '1', and Player 2(the left bat) can adjust with '2'. 
      - In addition, if you want to tune the AI bat's length, please tune player1 bat's length first. Then, switch the game mode to the AI mode.
      - 'Q' can be used to enter the game.
    - In the game scene,
      - Player 1(the right bat) can control the bat by 'O' and 'L' in a player-to-player game.
      - Player 2(the left bat) can control the bat by 'W' and 'S'.
      - 'Space'can be used to reset the ball.
      - 'Q' can be used to return back to the instruction scene.
      - When either one of the players wins a single matchup, the scoreboard will give you one point.
      - When either one of the players becomes the first one to score 11 points, the game will move to the end scene.
    -In the end scene,
      - The screen will show which player wins the game.
      - 'Q' can be used to switch to the instruction scene.


