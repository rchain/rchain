#!/usr/bin/python

import getpass
import github
import json
import os
import os.path
import pip
import subprocess
import sys

home = os.path.expanduser("~")
configPath = home + '/.pr_script_config.json'
scriptPath = os.path.realpath(__file__)


def requirePass():
    return getpass.getpass("Type your Github password: ")


def requireBase():
    return raw_input("Please specify the base branch you want the changes pulled into: ")


def requireTicketId():
    return raw_input("Type JIRA ticket's id: ")


def requireTicketHeader():
    return raw_input("Type JIRA ticket's description: \n")


def requireBody():
    return raw_input("Type PR's body: \n")


def requireRemote():
    remote = raw_input("Please, specify the repository for PR's in format ':owner/:repo': \n")
    return remote.lower()


def requireFeatureName():
    return raw_input("Type name of the feature you will work on: ").replace(" ", "-")


def mapDict(f,d):
    return dict(map(f, d.iteritems()))


def validateValues(items):
    (k,v) = items
    if v is None:
        print("Cannot infer the {0} value".format(k))
        newV = raw_input("Please specify a value for the {0}: ".format(k))
        return (k,newV)
    else:  
        return (k,v)

        
def execStdout(cmd):
    try:
        return subprocess.check_output(cmd)
    except subprocess.CalledProcessError:
        pass


def mapOptional(x,f):
    return None if x is None else f(x)


def gitName():
    name = execStdout(["git", "config", "user.name"])
    return mapOptional(name, lambda x: x.strip())


def gitCurrentBranch():
    return execStdout(["git", "rev-parse", "--abbrev-ref", "HEAD"]).strip()


def gitOrigin():
    origin =  execStdout(["git", "config", "remote.origin.url"])
    return mapOptional(origin, lambda x: x.strip())


def readJson(name):
    try:
        with open(name, 'a+') as f:
            return json.load(f)
    except ValueError:
        return {}


def dictMerge(l,r):
    res = l.copy()
    res.update(r.iteritems())
    return res


def writeJson(dict, name):
    with open(name, 'w') as f:
        json.dump(dict, f)

    
def execCmd(args):
    return subprocess.call(args)



class bcolors:
    WARNING = '\033[93m'
    ENDC = '\033[0m'


    
class Config:
    nameField = 'githubName'
    remoteField = 'remote'
    
    def __init__(self):
        globalConfig = self.loadConfig()
        
        if globalConfig.get(scriptPath, None) is None:
            newConfig = self.createConfig()
            self.globalConfig = dictMerge(globalConfig, newConfig)
            writeJson(self.globalConfig, configPath)
        else:
            self.globalConfig = globalConfig

        self.setupConfig()

        
    def setupConfig(self):
        self.config = self.globalConfig[scriptPath]
        self.name = self.config[self.nameField]
        self.remote = self.config[self.remoteField]
        

    def loadConfig(self):
        try:
            return readJson(configPath)
        except TypeError:
            return {}


    def createConfig(self):
        print "No configuration file found. Starting to create a new configuration file."
        
        data = {self.nameField: gitName()}
        checkedDict = mapDict(validateValues, data)
        checkedDict[self.remoteField] = requireRemote()
        globalConfig = { scriptPath: checkedDict }
        
        return globalConfig

    
    
class Tasks:
    prWarningMessage = bcolors.WARNING \
        + "Something went wrong while trying to create PR. " \
        + "Please make sure that current branch differs from the 'base' and no PR was created before." \
        + bcolors.ENDC
        
    def __init__(self, config):
        self.name = config.name
        self.remote = config.remote

        
    def createBranch(self):
        branchName = "dev-{0}-{1}".format(self.name, requireFeatureName())
        return execCmd(["git", "checkout", "-b", branchName])

        
    def createPr(self):
        print "Creating PR. Make sure that you push recent changes to the origin."

        repoName = self.remote
        username = self.name
        password = requirePass()
        g = github.Github(username, password)

        for repo in g.get_user().get_repos():
            if repo.full_name.lower() == repoName:
                self.checkAndMakePr(repo)


    def requireTitle(self):
        ticketId = requireTicketId()
        ticketHeader = requireTicketHeader()
        return "JIRA# {0}: {1}".format(ticketId, ticketHeader)

    
    def getHead(self):
        return "{0}:{1}".format(self.name, gitCurrentBranch())
    
                
    def requirePrParams(self):
        base = requireBase()
        head = self.getHead()
        title = self.requireTitle()
        body = requireBody()

        return (title, body, base, head)

    
    def checkAndMakePr(self, repo):
        if not self.test() is 0:
            raise ValueError("Some tests are failed.")

        params = self.requirePrParams()

        try:
            repo.create_pull(*params)
        except github.GithubException as e:
            print self.prWarningMessage
            raise ValueError(e)
        
        print "Success"
        

    def defaultScenario(self):
        print 'Not a supported task. Use "help" command for help.'

        
    def buildSnapshot(self):
        print "Start building snapshot.."
        return execCmd(["sbt", "package"])

    
    def test(self):
        print "Start testing.."
        return execCmd(["sbt", "test"])

    
    def noArgs(self):
        print 'No arguments specified. Use "help" command for help.'

        
    def help(self):
        print  """Commands:
        pr       - create a pull request to the 'origin'
        test     - run sbt tests
        branch   - create a new feature branch
        snapshot - build jar snapshot"""
        
    
    tasks = {
        'snapshot': buildSnapshot,
        'branch': createBranch,
        'test': test,
        'help': help,
        'pr': createPr
    }


    def executeTask(self, args):
        if len(args) == 0:
            self.noArgs()
        else:
            taskName = args[0]        
            task = self.tasks.get(taskName, Tasks.defaultScenario)
            task(self)

            
def main():
    gitPython = "PyGitHub"
    
    config = Config()
    print "Config path is: ", configPath
    print "Config content is:\n", config.config
    tasks = Tasks(config)
    tasks.executeTask(sys.argv[1:])
    

if __name__ == "__main__":
    main()
