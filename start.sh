#!/bin/bash

NODE_NAME=maparser

CONFIG_FILE=${NODE_NAME}
MARK_FILE="./logs/mark.log"
SMP=auto
ERL_PROCESSES=1024000

DATETIME=`date "+%Y%m%d%H%M%S"`

LOG_PATH="./logs/${NODE_NAME}_$DATETIME.log"
LOG_DIR="./logs"

ARGS=
while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
    -port) PORT=$1; shift ;;
        --) break ;;
        *) ARGS="$ARGS $PARAM" ;;
    esac
done

live()
{
    echo "--------------------------------------------------------------------"
    echo ""
    echo "重要提示: Maparser 将会以交互式模式启动"
    echo "所有的消息都会被直接打印在终端上."
    echo ""
    echo "如果想退出该模式请输入 q()，然后回车"
    echo ""
    echo "--------------------------------------------------------------------"
    echo "任意键继续"
    read foo
    mkdir -p ${LOG_DIR}/back
    mv ${LOG_DIR}/*.log ${LOG_DIR}/back
    erl +P ${ERL_PROCESSES} \
        +t 2048576 \
        -smp ${SMP} \
        -env ERL_MAX_PORTS 65535 \
        -pa ./ebin  ./Emysql/ebin\
        -sname ${NODE_NAME} \
        -boot start_sasl \
        -config ${CONFIG_FILE} \
        -kernel error_logger \{file,\"$LOG_PATH\"\} \
        -s maparser_app start_app
}


help()
{
    echo "--------------------------------------------------------------------"
    echo ""
    echo "管理命令:"
    echo " live             以交互方式启动节点"
    echo ""
    echo "命令行参数，如: ./map.sh live"
    echo ""
    echo "--------------------------------------------------------------------"
}

case $ARGS in
    ' live') live;;
    *) help;;
esac

