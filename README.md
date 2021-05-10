# CS583_Final Project
# Yinchao Zhu, zhuyin@oregonstate.edu
# Haoyuan Qiu, qiuha@oregonstate.edu
# Shukan Nieh, niehsh@oregonstate.edu

# [A brief description of your project goals (you can take this from your project proposal), and your current progress toward achieving those goals.]
# Our project goal is to develop a ping-pong game using Haskell. At this point, the game can run correctly, including move the bat, move the ball, the bat can rebound the ball correctly. We also have achieved the framework of switching different scenes. Although the count board cannot show the real-time score right now, we decide to achieve that in the next milestone. And the difficulty extension part which can change the difficulty by modifying the ball’s speed and the length of the bats are still under development.


# [Instructions for how to run your project (e.g. which file to load in GHCi), including several example inputs, if applicable.]
# 1. stack is needed for our project. You can visit the following website to install.
#    https://docs.haskellstack.org/en/stable/install_and_upgrade/
#    Windows users can click “Windows 64-bit Installer” to install manually.
# 2. Once you have successfully installed it, you can use your terminal cd to the root directory of our project and type “stack build” to compile.
# 3. After that, you can type “stack run” to enjoy our PingPong Game.

# [Notes]
# By now, the scoreboard and winning info. of our game are still under construction.
# Some of you may encounter a missing file / unknown error when running “stack build”.
# One possible solution is:
# Copy the glut32.dll we provided in the root directory to /root/.stack-work/install/(some magic words)/bin, along with the cs583-exe.exe
# This makes sense when we test :)


# [In Milestone #1: a list of 2–4 design questions that you have about your project, that you would like to discuss during the workshop.]
# 1. When we were trying to implement the difficulty into our program, we met a problem that the speed change may cause the function of bat collision to fail. 
#    We found the reason is that the position of the ball may not equal to the bat position. Therefore, our collision function won’t consider the ball touching the bat.
# 2. Because it’s the first version of the program, we are trying to make sure the scheme of the game is workable. For example, defining the game state, changing the scene of the game, creating the counting board, controlling bats’ position, and moving the ball. However, many logical functions have similar types, such as the judgement of the bat collision, the judgement of the wall collision, and the judgement of the winner. It’s better to define an extendible class to make the whole program look more structural. 
# 3. Using artificial intelligence to control one of the bats would be a challenge for us because there are still some mechanisms and modifications of the recent functions that need to be done.
