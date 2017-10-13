#!/usr/bin/python

import sys, os, json, subprocess, pip, getpass
import __builtin__

from github import Github
from os.path import expanduser


FileNotFoundError = getattr(__builtin__,"IOError","FileNotFoundError")

home = expanduser("~")
configPath = home + '/.pr_script_config.json'
scriptPath = os.path.realpath(__file__)


def requirePass():
    return getpass.getpass("Type your Github password: ")


def requireBase():
    return raw_input("Please specify the base branch you want the changes pulled into: ")


def requireTickedId():
    return raw_input("Type JIRA ticked's id: ")


def requireTickedHeader():
    return raw_input("Type JIRA ticked's description: \n")


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
        newV = raw_input("Please specify a value for the : {0}".format(k))
        return (k,newV)
    else:  
        return (k,v)

    
def readFile(name):
    f = open(name, 'a+')
    content = f.read()
    f.close()
    return content

        
def execStdout(cmd):
    try:
        return subprocess.check_output(cmd)
    except subprocess.CalledProcessError:
        None


def mapOptional(x,f):
    if not x is None:
        return f(x)
    else:
        x


def gitName():
    name = execStdout(["git", "config", "user.name"])
    mapOptional(name, lambda x: x.stript())


def gitCurrentBranch():
    return execStdout(["git", "rev-parse", "--abbrev-ref", "HEAD"]).strip()


def gitOrigin():
    origin =  execStdout(["git", "config", "remote.origin.url"])
    mapOptional(origin, lambda x: x.strip())


def parseJson(content):
    try:
        return json.loads(content)
    except ValueError:
        return {}

    
def optionalGet(dict,k):
    if dict is None:
        None
    else:
        dict.get(k, None)


def dictMerge(l,r):
    res = l.copy()
    res.update(r.iteritems())
    return res


def writeJson(dict, name):
    f = open(name, 'w')
    content = json.dumps(dict)
    f.write(content)
    f.close()

    
def execCmd(args):
    return subprocess.call(args)
                       

    
class Config:
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
        for k,v in self.config.items():
            setattr(self, k, v)
        

    def loadConfig(self):
        try:
            content = readFile(configPath)
            globalConfig = parseJson(content)
            return globalConfig
        except TypeError:
            {}


    def createConfig(self):
        data = {'githubName': gitName()}
        checkedDict = mapDict(validateValues, data)
        checkedDict['remote'] = requireRemote()
        globalConfig = { scriptPath: checkedDict }
        
        return globalConfig

    
    
class Tasks:
    def __init__(self, config):
        self.config = config

        
    def createBranch(self):
        branchName = "dev-{0}-{1}".format(self.config.name, requireFeatureName())
        execCmd(["git", "checkout", "-b", branchName])

        
    def createPr(self):
        print "Creating PR. Make sure that you push recent changes to the origin"

        repoName = self.config.remote
        username = self.config.name
        password = requirePass()
        github = Github(username, password)

        for repo in github.get_user().get_repos():
            if repo.full_name.lower() == repoName:
                self.checkAndMakePr(repo)


    def requireTitle(self):
        tickedId = requireTickedId()
        tickedHeader = requireTickedHeader()
        return "JIRA# {0}: {1}".format(tickedId, tickedHeader)

    
    def getHead(self):
        return "{0}:{1}".format(self.config.name, gitCurrentBranch())
    
                
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

        repo.create_pull(*params)
        
        print "Success"
        

    def defaultScenario(self):
        print 'Not a supported task. Use "help" command for help.'

        
    def buildSnapshot(self):
        "Start building snapshot.."
        return execCmd(["sbt", "package"])

    
    def test(self):
        print "Start testing.."
        return execCmd(["sbt", "test"])

    
    def noArgs(self):
        print 'No arguments specified. Use "help" command for help.'

        
    def help(self):
        print  """Commands:
        pr       - create a pull request to the 'origin'"
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
    print"Config is", config.config
    tasks = Tasks(config)
    tasks.executeTask(sys.argv[1:])
    

if __name__ == "__main__":
    main()
