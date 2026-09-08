package xscache.oceanus.compactchi


case class CCHIParameters (

    // CCHI Essentials
    /*
    TxnID_Width: Width of the upstream transaction ID.
                 Decided by the upstream transaction outstanding capability.
    */
    TxnID_Width             : Int               = 8,

    /*
    DBID_Width: Width of the downstream transaction ID.
                Decided by the downstream transaction outstanding capability.
    */
    DBID_Width              : Int               = 8,

    /*
    WayIndex_Width: Width of the way index.
                    Decided by the maximum number of ways in the downstream cache.
    */
    WayIndex_Width          : Int               = 2,

    /*
    UpstreamNodeID_Width: Width of the upstream node ID.
                          Decided by the maximum number of upstream nodes and node ID mapping in the system.
    */
    UpstreamNodeID_Width     : Int              = 4,

    /*
    DownstreamNodeID_Width: Width of the downstream node ID.
                            Decided by the maximum number of downstream nodes and node ID mapping in the system.
    */
    DownstreamNodeID_Width   : Int              = 4,

    /* 
    Data_Width: Width of the data bus.
                Decided by the actual data width between components.
    */
    Data_Width               : Int              = 256,

    /*
    TagAlias_Width: Width of the tag alias.
                    Decided by the upstream VIPT cache capacity.
    */
    TagAlias_Width            : Int             = 2,

    /*
    UWPersist_Enable: Enabling support for Upstream Way Persistence. 
    */
    UWPersist_Enable          : Boolean         = true,

    /*
    UWPredict_Enable: Enabling support for Upstream Way Prediction.
    */
    UWPredict_Enable          : Boolean         = true
)
