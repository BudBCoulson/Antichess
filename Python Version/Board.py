import Pieces
import Rules
import Move
import sys

class Board:
	pieces = []
	WHITE = 0
	BLACK = 1
	movesMade = []
	doublePawnPush = []
	madeEnPassant = []
	
	def __init__(self, textmode=False):
		self.textmode = textmode
		self.pieces.append(Pieces.Rook(self.BLACK))
		self.pieces.append(Pieces.Knight(self.BLACK))
		self.pieces.append(Pieces.Bishop(self.BLACK))
		self.pieces.append(Pieces.Queen(self.BLACK))
		self.pieces.append(Pieces.King(self.BLACK))
		self.pieces.append(Pieces.Bishop(self.BLACK))
		self.pieces.append(Pieces.Knight(self.BLACK))
		self.pieces.append(Pieces.Rook(self.BLACK))

		for i in range(9,17):
			self.pieces.append(Pieces.Pawn(self.BLACK))

		for i in range(17, 65-16):
			self.pieces.append(None)

		for i in range(65-16,65-8):
			self.pieces.append(Pieces.Pawn(self.WHITE))

		self.pieces.append(Pieces.Rook(self.WHITE))
		self.pieces.append(Pieces.Knight(self.WHITE))
		self.pieces.append(Pieces.Bishop(self.WHITE))
		self.pieces.append(Pieces.Queen(self.WHITE))
		self.pieces.append(Pieces.King(self.WHITE))
		self.pieces.append(Pieces.Bishop(self.WHITE))
		self.pieces.append(Pieces.Knight(self.WHITE))
		self.pieces.append(Pieces.Rook(self.WHITE))

	def clear(self):
		"""Remove all pieces and moves."""
		for i in range(0,64):
			self.pieces[i] = None
		self.movesMade = []
		self.doublePawnPush = []
		self.madeEnPassant = []

	def stringToSquare(self, squareString):
		# E.g. squareString = e2
		conv = dict(a=0, b=1, c=2, d=3, e=4, f=5, g=6, h=7)
		row = 7 - (int(squareString[1]) - 1)
		col = conv[squareString[0]]
		return row*8 + col

	def setPiece(self, squareString, piece):
		square = self.stringToSquare(squareString)
		self.pieces[square] = piece

	def display(self):
		if self.textmode:
			self.displayAsText()
		else:
			self.displayAsUnicode()

	def displayAsText(self):
		lastMove = self.getLastMove()
		print("--a-b-c-d-e-f-g-h--")
		for row in range(8):
			sys.stdout.write(str(9 - (row+1)))
			sys.stdout.write("|")
			for col in range(8):
				idx = 8*row + col
				p = self.pieces[idx]
				if (row==lastMove[0][0] and col==lastMove[0][1]) or (row==lastMove[1][0] and col==lastMove[1][1]):
					if p==None:
						#sys.stdout.write(".")
						sys.stdout.write(Pieces.bcolours.PIECECOLOURALT[ self.getLastMovedPiece().colour  ] +  "." + Pieces.bcolours.ENDC,end='')
					else:
						p.displayAsText(alt=True)
				else:
					if p==None:
						sys.stdout.write(" ")
					else:
						p.displayAsText()
				sys.stdout.write("|")
			print(9-(row+1))
		print("--a-b-c-d-e-f-g-h--")

	def displayAsUnicode(self):
		lastMove = self.getLastMove()
		print("  a b c d e f g h")
		for row in range(8):
			sys.stdout.write(str(9 - (row+1)))
			#sys.stdout.write("|")
			sys.stdout.write(" ")
			for col in range(8):
				idx = 8*row + col
				p = self.pieces[idx]
				squarecolour = (col + row)%2
				if (row==lastMove[0][0] and col==lastMove[0][1]) or (row==lastMove[1][0] and col==lastMove[1][1]):
					if p==None:
						print(Pieces.bcolours.PIECECOLOURALT[ self.getLastMovedPiece().colour  ] + Pieces.bcolours.BGCOLOURALT[squarecolour] + ". " + Pieces.bcolours.ENDC,end='')
					else:
						p.displayAsUnicode(squarecolour=squarecolour, alt=True)
				else:
					if p==None:
						print(Pieces.bcolours.BGCOLOUR[squarecolour] + "  " + Pieces.bcolours.ENDC,end='')
					else:
						p.displayAsUnicode(squarecolour=squarecolour)
			sys.stdout.write(" ")
			print(9-(row+1))
		print("  a b c d e f g h")

	def makeMove(self, move):
		# Allow null moves (passes)
		if move==Move.PASS:
			return
                # Retractions and resignations should not be made here
		if move==Move.RETRACT or move==Move.RESIGN:
			print("Move error.")
			exit()

		fr, to = move.unpack()
		self.movesMade.append( [move, self.pieces[to]] )

		# Record double pawn pushes for en passant
		if isinstance(self.pieces[fr], Pieces.Pawn) and abs(move.to[0]-move.fr[0])==2:
				self.doublePawnPush.append(True)
		else:
				self.doublePawnPush.append(False)
		if move.isEnpassant(self):
				self.madeEnPassant.append(True)
				# The captured pawn will be in same column, but one row behind
				# We don't store it explicitly.
				# This is ok because if we want to retract the move, we know it's
				# an en passant and therefore which piece to replace and where.
				capturedPieceCol = move.to[1]
				# Offset is -1 for black, +1 for white
				if self.pieces[fr].colour==self.WHITE:
						offset = +1
				else:
						offset = -1
				capturedPieceRow = move.to[0] + offset
				self.pieces[ capturedPieceRow*8 + capturedPieceCol ] = None
		else:
				self.madeEnPassant.append(False)

		self.pieces[ to ] = self.pieces[ fr ]
		self.pieces[ fr ] = None
		if isinstance(move, Move.PromotionMove):
			self.pieces[ to ] = move.promoteTo

	def retractMove(self):
		if len(self.movesMade)==0:
			return
		[move, piece] = self.movesMade.pop()
		fr, to = move.unpack()
		self.pieces[ fr ] = self.pieces[ to ]
		# Put captured piece back in the correct place in case of en passant
		if self.madeEnPassant[-1]:
			capturedPieceCol = move.to[1]
			# Offset is -1 for black, +1 for white
			if self.pieces[fr].colour==self.WHITE:
					offset = +1
			else:
					offset = -1
			capturedPieceRow = move.to[0] + offset
			self.pieces[ capturedPieceRow*8 + capturedPieceCol ] = Pieces.Pawn( 1-self.pieces[fr].colour )
		else:
			self.pieces[ to ] = piece
		if isinstance(move, Move.PromotionMove):
			self.pieces[ fr ] = Pieces.Pawn( self.pieces[fr].colour )
			self.doublePawnPush.pop()
			self.madeEnPassant.pop()

	def retractTurn(self):
		# Try to pop two moves, to get back to the same player
		if len(self.movesMade)<2:
			return False
		self.retractMove()
		self.retractMove()
		return True

	def getLastMove(self):
		if len(self.movesMade)==0:
			return Move.NONE
		return self.movesMade[-1][0]

	def getSecondLastMove(self):
		if len(self.movesMade)<2:
			return Move.NONE
		return self.movesMade[-2][0]
	
	def getLastMovedPiece(self):
		if len(self.movesMade)==0:
			return None
		fr, to = self.getLastMove().unpack()
		return self.pieces[to]

	def hasCaptures(self, colour):
		# Important that we don't enforce captures here, otherwise enter infinite loop
                _, isCapture = Rules.Suicide().getAllValidMoves(self, colour, enforceCaptures=False)
                return sum(isCapture)>0

	def hasPieceOn(self, row, col):
		return not (self.pieces[row*8+col] == None)

	def getPieceOn(self, m):
		row = m[0]
		col = m[1]
		return self.pieces[row*8+col]

	def hasClearPath(self, fr, to):
		def sign(x):
			if x > 0:
				return 1
			if x < 0:
				return -1
			return 0
		# check for bishop move first
		if abs(fr[0]-to[0]) == abs(fr[1]-to[1]):
			if fr[1]>to[1]:
				fr, to = to, fr
			s = [sign(to[0] - fr[0]), sign(to[1]-fr[1])]
			for inc in range(1, to[1]-fr[1]):
				if self.hasPieceOn(fr[0]+s[0]*inc, fr[1]+s[1]*inc):
					return False
			return True
		# Now straight-line moves
		if fr[0]==to[0]:
			if fr[1]>to[1]:
				fr, to = to, fr
			for col in range(fr[1]+1,to[1]):
				if self.hasPieceOn(fr[0], col):
					return False
		if fr[1]==to[1]:
			if fr[0]>to[0]:
				fr, to = to, fr
			for row in range(fr[0]+1,to[0]):
				if self.hasPieceOn(row, fr[1]):
					return False
		# Anything else, just return True
		return True

	def getAllPieces(self, colour):
		pieces = []
		for row in range(0, 8):
			for col in range(0, 8):
				if self.hasPieceOn(row, col):
					p = self.getPieceOn([row,col])
					if p.colour==colour:
						pieces.append( [row,col] )
		return pieces

	def getNumPieces(self, colour):
		return len( self.getAllPieces(colour) )
















