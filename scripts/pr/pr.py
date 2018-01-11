#!/usr/bin/python

import argparse
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


def dictDel(dict, key):
    if key in dict:
        r = dict.copy()
        del r[key]
        return r
    else:
        return dict


def showConfig(config):
    print "Config path is: ", configPath
    print "Config content is:\n", config.config


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


def orElse(x, lazy):
    if x is None:
        return lazy()
    else:
        return x


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
            self.saveConfig()
        else:
            self.globalConfig = globalConfig

        self.setupConfig()

        
    def setupConfig(self):
        self.config = self.globalConfig[scriptPath]

        for (k,v) in self.config.items():
            setattr(self, k, v)
        

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


    def set(self, args):
        print 'Not implemented yet.'


    def saveConfig(self):
        writeJson(self.globalConfig, configPath)


    def storeVariables(self, **args):
        newConfig = dictMerge(self.config, args)
        self.globalConfig[scriptPath] = newConfig
        self.saveConfig()
        self.setupConfig()

    
    
class Tasks:
    prWarningMessage = bcolors.WARNING \
        + "Something went wrong while trying to create PR. " \
        + "Please make sure that current branch differs from the 'base' and no PR was created before." \
        + bcolors.ENDC
        
    def __init__(self, config):
        self.name = config.githubName
        self.remote = config.remote

        
    def createBranch(self, featureName):
        branchName = "dev-{0}-{1}".format(self.name, featureName)
        return execCmd(["git", "checkout", "-b", branchName])


    def getRepo(self):
        repoName = self.remote
        username = self.name
        password = requirePass()

        g = github.Github(username, password)

        for repo in g.get_user().get_repos():
            if repo.full_name.lower() == repoName:
                return repo


    def merge(self, prNumber, msg):
        repoName = self.remote

        repo = self.getRepo()
        pr = repo.get_pull(prNumber)
        pr.merge(msg)


    def createPr(self, params):
        print "Creating PR. Make sure that you push recent changes to the origin."

        repo = self.getRepo()

        return self.checkAndMakePr(repo, params)

    
    def getHead(self):
        return "{0}:{1}".format(self.name, gitCurrentBranch())

    
    def checkAndMakePr(self, repo, params):
        if not self.test() is 0:
            print "Some tests are failed, abort PR."
            sys.exit(1)

        try:
            return repo.create_pull(*params)
        except github.GithubException as e:
            print e
            print self.prWarningMessage
            sys.exit(1)
        
        print "Success"
        
        
    def buildSnapshot(self):
        print "Start building snapshot.."
        return execCmd(["sbt", "package"])

    
    def test(self):
        print "Start testing.."
        return execCmd(["sbt", "test"])


    def commit(self, message):
        print "Committing changes.."
        return execCmd(["git", "commit", "-m", message])


    def push(self):
        print "Pushing changes.."
        branchName = gitCurrentBranch()
        return execCmd(["git", "push", "--set-upstream", "origin", branchName])


    def executeTask(self, args):
        if len(args) == 0:
            self.noArgs()
        else:
            taskName = args[0]        
            task = self.tasks.get(taskName, Tasks.defaultScenario)
            task(self)



class HierarchicParserBuilder:
    def __init__(self, routes):
        self.parser = self.buildParser(routes)

    def buildParser(self, routes):
        p = argparse.ArgumentParser(description='Helper tool')

        def helper(parser, structure):
            func = structure.pop('func', None)
            args = structure.pop('args', {})

            for (name,params) in args.items():
                parser.add_argument(name, **params)

            if func:
                parser.set_defaults(func=func)

            subparser = None

            for (name,subStruct) in structure.items():
                subparser = orElse(subparser, parser.add_subparsers)

                help = subStruct.pop('help', None)
                parserForName = subparser.add_parser(name, help=help)
                helper(parserForName, subStruct)

            return subparser

        helper(p, routes)
        return p


    def parse(self, args=None):
        r = self.parser.parse_args(args)
        return r.func(r)



class Workflow:

    def __init__(self, config, tasks):
        self.config = config
        self.tasks = tasks
        self.commitMsg = {
            'comment': self.commentCommit,
            'transition': self.transitionCommit
        }


    def commentCommit(self, msg, ticketId):
        return "{0} {1} #comment PR created".format(msg, ticketId)


    def transitionCommit(self, msg, ticketId):
        return "{0} {1} #inreview PR created".format(msg, ticketId)


    def doneCommit(self, ticketId):
        return "{0} #done Merged".format(ticketId)


    def storeArgs(self, args, **add):
        d = dictDel(args.__dict__, 'func')
        self.config.storeVariables(**dictMerge(d,add))


    def init(self, args):
        self.storeArgs(args, phase='init')
        featureName = '-'.join(args.title.lower().split())
        print "Feature name"
        print featureName
        self.tasks.createBranch(featureName)


    def commit(self, args):
        hasPhase = lambda obj: hasattr(obj, 'phase')
        isInitPhase = lambda obj: obj.phase == 'init'

        if not (hasPhase(self.config) and isInitPhase(self.config)):
            print "The 'phase' parameter is does not setup in configuration file. " + \
                "Probably you should use 'project init' command first."
            sys.exit(1)

        ticketId = self.config.ticket
        message = self.commitMsg[args.type](args.message, ticketId)
        self.tasks.commit(message)
        self.config.storeVariables(phase='commit')


    def prParams(self, args):
        ticketId = self.config.ticket
        ticketHeader = self.config.title
        title = "JIRA# {0}: {1}".format(ticketId, ticketHeader)
        head = self.tasks.getHead()
        body = orElse(args.body, str)

        return (title, body, args.base, head)


    def pr(self, args):
        params = self.prParams(args)

        self.tasks.push()
        pr = self.tasks.createPr(params)
        self.config.storeVariables(prNumber = pr.number)


    def merge(self, args):
        ticketId = self.config.ticket
        msg = self.doneCommit(ticketId)
        pr = self.config.prNumber
        self.tasks.merge(pr, msg)

        print "Merged successfully"



def main():
    gitPython = "PyGitHub"

    config = Config()
    tasks = Tasks(config)
    workflow = Workflow(config, tasks)


    dict = {
        'config': {
	    'help': 'Config help',
	    'show': {
		'help': 'this command is used to print config file contents and location',
		'func': lambda args: showConfig(config)
	    },
            'set': {
		'help': 'this command is used to set up config variables',
		'func': lambda args: config.set(args),
                'args': {
                    '-name': {
                        'help': 'set username for the current config'
                    },
                    '-origin': {
                        'help': 'set origin for the current config'
                    }
                }
	    }
        },
        'project': {
            'help': "Commands for project's workflow transitions",
            'init': {
                'help': 'Initiate new workflow with specified feature',
                'func': lambda args: workflow.init(args),
                'args': {
                    '-title': {
                        'help': 'Name of the feature you will work on',
                        'required': True
                    },
                    '-ticket': {
                        'help': "JIRA's ticket id",
                        'required': True
                    }
                }
            },
            'commit': {
                'help': 'Commit Changes Using Git with service information',
                'func': lambda args: workflow.commit(args),
                'args': {
                    '-message': {
                        'help': 'Commit message',
                        'required': True
                    },
                    '-type': {
                        'help': """The type of the commit.
                    'comment' leaves a commentary at JIRA's ticket and
                    'transition' set ticket's status to 'In Review'""",
                        'required': True,
                        'choices': ['comment', 'transition']
                    }
                }
            },
            'pr': {
                'help': 'Manage pull requests',
                'create': {
                    'help': 'Create pull request for current branch',
                    'func': lambda args: workflow.pr(args),
                    'args': {
                        '-base': {
                            'help': 'The base branch you want the changes pulled into',
                            'required': True
                        },
                        '-body': {
                            'help': "PR's text body"
                        }
                    }
                },
                'merge': {
                    'help': 'Merge last pull request',
                    'func': lambda args: workflow.merge(args),
                }
            }
        },
        'build': {
            'help': 'Build jar snapshot',
            'func': lambda args: tasks.buildSnapshot()
        },
        'test': {
            'help': 'Run sbt tests',
            'func': lambda args: tasks.test()
        }
    }

    p = HierarchicParserBuilder(dict)
    p.parse()


if __name__ == "__main__":
    main()
