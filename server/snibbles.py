import tornado.websocket
import tornado.ioloop
import threading
import time
import json


class GameThread(threading.Thread):

  def __init__(self, playerList):
    threading.Thread.__init__(self)
    self.playerList = playerList

  def getAllMessages(self):
    messages = []
    for player in playerList:
      message = player.getLatestMessage()
      messages.append(message)
    return messages

  def run(self):
    while True:
      message = "[" + ", ".join(self.getAllMessages()) + "]"
      #message = self.getAllMessages()
      print message

      for player in playerList:
        player.write_message(message)

      time.sleep(0.125)

playerNumber = 0
playerList = []

class PlayerSocket(tornado.websocket.WebSocketHandler):

    def __init__(self, application, request, **kwargs):
      global playerList
      global playerNumber

      super(PlayerSocket, self).__init__(application, request)
      self.playerList = playerList
      self.latestMessage = ""
      self.number = playerNumber
      playerNumber = 1 #playerNumber + 1

    def open(self):
      self.playerList.append(self)
      print("WebSocket opened")

    def on_message(self, message):
      self.latestMessage = message
      print message

    def getLatestMessage(self):
      return '{ "id": ' + str(self.number) + ', "msg": "' + self.latestMessage + '"}'

    def clearLatestMessage(self):
      self.latestMessage = ""

    def on_close(self):
    	if self in playerList:
    		self.playerList.remove(self)

        print("WebSocket closed")

    def check_origin(self, origin):
        return True


if __name__ == "__main__":

    gameThread = GameThread(playerList)
    gameThread.start()

    port = 9000

    app = tornado.web.Application(
        [(r"/", PlayerSocket)
        ])
    app.listen(port)

    print "Listening on ", port
    tornado.ioloop.IOLoop.current().start()

