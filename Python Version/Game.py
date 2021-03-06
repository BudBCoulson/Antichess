#!/usr/bin/python 

#from rules import suicide
import Board
import Player
import Rules
import Move

from optparse import OptionParser
#import argparse
import time

playerNames = ["White", "Black"]

def playGame():
	parser = OptionParser()
	parser.add_option("-d", "--depth", type="int", dest="AIdepth", default=99,
	                  help="set maximum AI search depth", metavar="MAXDEPTH")
	parser.add_option("-w", "--white", dest="white", default="human",
	                  help="set white player", metavar="PLAYER")
	parser.add_option("-b", "--black", dest="black", default="ai",
	                  help="set black player", metavar="PLAYER")
	parser.add_option("-t", "--time", type="int", dest="maxTime", default=5,
	                  help="set maximum AI thinking time", metavar="MAXTIME")
	parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False,
	                  help="set verbose AI")
	parser.add_option("-s", "--simple", action="store_true", dest="textmode", default=False,
	                  help="use simple board display")
	parser.add_option("--singleturn", action="store_true", dest="singleturn", default=False,
	                  help="(debug) play a single turn only")
	(options, args) = parser.parse_args()

	b = Board.Board()#textmode = options.textmode)
	AIdepth = options.AIdepth
	maxTime = options.maxTime

	playertype = [options.white, options.black]
	players = []
	for i in [0,1]:
		p = playertype[i]
		if p=="human":
			players.append( Player.HumanPlayer(i) )
		elif p=="ai":
			players.append( Player.AIPlayer(i, AIdepth, options.verbose) )
		elif p=="random":
			players.append( Player.RandomPlayer(i) )
		elif p=="pass":
			players.append( Player.PassingPlayer(i) )
		else:
			#TODO exception
			print("Error: unknown player type:", p)
			exit()

		print("%s is %s." %(playerNames[i], players[i].name)),
		if p=="ai":
			print("Depth is %s, max thinking time is %ds." % (AIdepth, maxTime))
		print()


	r = Rules.Suicide()
	b.display()
	print()

	WIN = -1
	while True:
		for col in [0, 1]:
			# First check for win
			numpieces = b.getNumPieces(col)
			valid, capture = r.getAllValidMoves(b, col)
			if numpieces==0 or len(valid)==0:
				WIN = col
				break

			madeValidMove = False
			startTime = time.time()
			while not madeValidMove:
				print(playerNames[col] + "'s turn")
				m = players[col].getMove(b, maxTime)
				# NoneMove -> try again
				if m==Move.NONE:
					continue
				# If retract, we pop two moves and try again
				if m==Move.RETRACT:
					lastMoveByThisPlayer = b.getSecondLastMove()
					if b.retractTurn():
						print(playerNames[col], "retracts move", lastMoveByThisPlayer)
					else:
						print("Unable to retract.")
						print()
						b.display()
						print()
						continue
				try:
					r.validate(m, b, col)
					madeValidMove = True
				except Rules.RulesViolation as e:
					madeValidMove = False
					print("Invalid move: " + e.value)
					print("Got valid move in ", time.time()-startTime, "s")
			if m==Rules.Move.RESIGN:
				print(playerNames[col] + " resigns. ")
				WIN = 1-col
				break
			b.makeMove(m)
			print()
			b.display()
			print(playerNames[col] + " moved", m)
			print()
			if options.singleturn:
				exit()
		if not WIN==-1:
			print(playerNames[WIN] + " wins!")
			exit()






