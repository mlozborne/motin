#########################################################################################
########################### Unit Testing ################################################
#########################################################################################


from Log import gLog
from StartAndKill import StartAndKill


def raw_input(st):
    return input(st)

if __name__ == "__main__":
    gLog.open('1', 5)
    sak = StartAndKill(mode='testing')
    sak.start("simulator")
    sak.start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
    sak.start("ut4", ip = "127.0.0.1", port = "1234")
    sak.start("RBLDisplay", ip = "127.0.0.1", port = "1235")
    sak.start("adminthrottle", ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    print("\n")
    sak.start("controller", trace = "no")
    sak.start("ut4", port = "1236")
    sak.start("RBLDisplay", ip = "127.1.1.1")
    sak.start("adminthrottle", ip = "127.0.0.1", port = "1234", logs = "yes")
    print("\n")
    sak.kill("sim")
    sak.kill("cont")
    sak.kill("ut4")
    sak.kill("RBL")
    sak.kill("admin")

    sak.kill()
    sak.kill("all")
    gLog.close()
