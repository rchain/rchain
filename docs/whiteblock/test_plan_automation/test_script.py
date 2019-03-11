# Test Script for RChain based on the test plan in this link:
# https://github.com/rchain/rchain/blob/dev/docs/whiteblock-test-plan.md
# This can be modifid later for a generic test automation

#!/usr/bin/env python
import subprocess
import sys
import os
import json
import time
import yaml
from optparse import OptionParser

class TestPlanWorker:
    def __init__(self, test_name, test_plan_file, test_period):
        self.test_name = test_name
        self.input_file_name = test_plan_file
        self.test_plan_yaml = {}
        self.test_period = test_period

    def initialize(self):
        print "Test Plan:", self.test_name
        print "Initialize test plan..."
        with open(self.input_file_name, 'r') as stream:
            try:
                self.test_plan_yaml = yaml.load(stream)
                # print self.test_plan_yaml
                # print yaml.dump(test_plan, default_flow_style=False)
                # print json.dumps(self.test_plan_yaml, indent=4, sort_keys=True)

            except yaml.YAMLError as exc:
                print exc
                print "[ERROR] Yaml file parsing failed"
        print "Finished initialization. \n"


    # Setup the blockchain network based on the case info
    def setup_test_case(self, case_params):
        print "Case Params:", case_params
        validator = case_params['validator']
        static_nodes = case_params['static_nodes']
        total_num_nodes = validator + static_nodes
        contract = case_params['contract']
        bandwidth = case_params['bandwidth']
        network_latency = case_params['network_latency']
        packetloss = case_params['packetloss']

        # Network Build
        print "Building the Network ..."
        print('whiteblock build -n ' + str(total_num_nodes) + ' -v ' + str(validator) + ' -b rchain -y')
        os.system('whiteblock build -n ' + str(total_num_nodes) + ' -v ' + str(validator) + ' -b rchain -y')
        print "Finish Building the Network."

        # Network Config
        print "Configuring Network ..."
        print('whiteblock netconfig delay ' + str(network_latency))
        os.system('whiteblock netconfig delay ' + str(network_latency))
        print('whiteblock netconfig loss ' + str(packetloss))
        os.system('whiteblock netconfig loss ' + str(packetloss))
        print('whiteblock netconfig bw ' + bandwidth)
        os.system('whiteblock netconfig bw ' + bandwidth)
        print('whiteblock netconfig on')
        os.system('whiteblock netconfig on')
        print "Finish Configuring Network."

        # Deploy smart contract
        # os.system('-----')

        print "Test Period: " + str(self.test_period) + " seconds. " + " Testing ..."


    # Reset the network configuration
    def reset_test_case(self, case_params):
        print "Reset Network Params ..."
        os.system('whiteblock netconfig delay 0')
        os.system('whiteblock netconfig loss 0')
        os.system('whiteblock netconfig bw 1 Gbps')
        os.system('whiteblock netconfig off')
        print "Reset Finished."



    # run the file series by series
    def run(self):
        tests = self.test_plan_yaml
        # print tests
        for test in tests['test_series']:
            series_id = test.keys()[0]
            series_name = test[series_id]['name']
            print "Current Series: ", series_id, series_name, "\n"

            for case in test[series_id]['cases']:
                case_id = case.keys()[0]
                print "\nCurrent Case:", case_id
                case_params = case[case_id]
                self.setup_test_case(case_params)
                # Run each test for self.test_period seconds
                time.sleep(float(self.test_period))
                self.reset_test_case(case_params)


        print self.test_name, "Complete!!!"


if __name__ == '__main__':

    optparser = OptionParser()
    optparser.add_option('-f', '--inputFile', dest='input', help='filename containing yaml')
    optparser.add_option('-n', '--testName', dest='test_name', help='teat name', default="RChain Perfromance Test")
    optparser.add_option('-p', '--testPeriod', dest='test_period', help='teat period', default=300)

    (options, args) = optparser.parse_args()


    inFile = None
    errorFlag = 0
    errorMsg = ''
    if options.input is None:
        errorFlag = 1
        errorMsg = "Please use -f to specify test plan file"
    elif options.input is not None:
        # check if file exist and is the correct file type
        if (not os.path.isfile(options.input) or not options.input.endswith('.yaml')):
            errorFlag = 1
            errorMsg = "Unable to find test plan; Test plan should be in a yaml file \n"


    if(errorFlag):
        print '[ERROR]' + errorMsg
        sys.exit('System will exit')
    else:
        inFile = options.input

    tp_workder = TestPlanWorker(options.test_name, inFile, options.test_period)
    tp_workder.initialize()
    tp_workder.run()
