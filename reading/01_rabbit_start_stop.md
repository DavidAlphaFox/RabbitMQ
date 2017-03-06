# RabbitMQ的启停
RabbitMQ的整个系统启动依然是遵循Erlang OTP应用规范，从rabbit.erl这个文件开始，按
照OTP application进行启动和停止。

# start函数
该函数首先确保以下几件事情

* 必要的application被加载到内存中
* 日志文件正常被打开
* 准备集群状态文件
* 检查mnesia的一致性
* 启动broker和相关插件，这个步骤中就用到我们前面说的rabbit_boot_step

start函数的核心操作都被封装在start_it这个函数当中，start_it函数是确保RabbitMQ启
动流程的唯一性。		
start_it函数会立刻创建一个进程，这个进程只接收一个简单的消息，接着将该进程注册为
rabbit_boot命名进程。请大家注意，这个过程是一个两部操作，也就是说，在进程创建后
注册为命名进程前，可能发生抢注的情况。		
当我们rabbit_boot进程注册成功后，会检查rabbit进程是否在当前节点上存在，如果存在
了就说明，该节点已经启动了。如果没有的话，就开始执行启动的步骤。
那么为什么不是直接注册一个rabbit进程呢？虽然可能性非常小，但是这里面其实是用了两步加锁的经典方案，防止
rabbit进程被重复的创建。
