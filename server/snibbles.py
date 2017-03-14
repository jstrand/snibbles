import tornado.websocket
import tornado.ioloop
import threading
import time


class GameThread(threading.Thread):

  def __init__(self, playerList):
    threading.Thread.__init__(self)
    self.playerList = playerList

  def getAndClearAllMessages(self):
    messages = []
    for player in playerList:
      messages += player.getLatestMessage()
      player.clearLatestMessage()
    return messages

  def run(self):
    while True:
      messages = self.getAndClearAllMessages()
      if messages == []:
        messages = ["T"]
      for player in playerList:
        for message in messages:
     	    player.write_message(message)

      time.sleep(0.125)


playerList = []

class PlayerSocket(tornado.websocket.WebSocketHandler):

    def __init__(self, application, request, **kwargs):
      super(PlayerSocket, self).__init__(application, request)
      self.playerList = playerList
      self.latestMessage = ""

    def open(self):
      self.playerList.append(self)
      print("WebSocket opened")

    def on_message(self, message):
      print message
      self.latestMessage = message

    def getLatestMessage(self):
      return self.latestMessage

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

