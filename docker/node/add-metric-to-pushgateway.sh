echo "some_metric 10" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job1
echo "some_metric_2 30" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job2
curl http://127.0.0.1:9091/metrics
