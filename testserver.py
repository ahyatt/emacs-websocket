import logging
import tornado
import tornado.web
from tornado import httpserver
from tornado import ioloop
from tornado import websocket


class EchoWebSocket(websocket.WebSocketHandler):


    def open(self):
        logging.info("OPEN")
        self.ping_callback = ioloop.PeriodicCallback(self.send_ping, 5000)
        self.ping_callback.start()

    def on_message(self, message):
        logging.info(u"ON_MESSAGE: {0}".format(message))
        self.write_message(u"You said: {0}".format(message))

    def on_close(self):
        logging.info("ON_CLOSE")

    def allow_draft76(self):
        return False

    def send_ping(self):
        logging.info(u"PING!")
        self.ping(b'')

    def on_pong(self, data):
        logging.info("PONG: {0}".format(data))

if __name__ == "__main__":
    import tornado.options
    tornado.options.parse_command_line()
    application = tornado.web.Application([
        (r"/", EchoWebSocket),
    ])
    server = httpserver.HTTPServer(application)
    server.listen(9999)
    logging.info("STARTED: Server start listening")
    ioloop.IOLoop.instance().start()
