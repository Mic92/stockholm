"""TS011F plug."""

from zigpy.profiles import zgp, zha
from zigpy.quirks import CustomDevice
from zigpy.zcl.clusters.general import (
    Basic,
    GreenPowerProxy,
    Groups,
    Identify,
    OnOff,
    Ota,
    Scenes,
    Time,
)
from zigpy.zcl.clusters.homeautomation import ElectricalMeasurement
from zigpy.zcl.clusters.lightlink import LightLink
from zigpy.zcl.clusters.measurement import TemperatureMeasurement
from zigpy.zcl.clusters.smartenergy import Metering

from zhaquirks.const import (
    DEVICE_TYPE,
    ENDPOINTS,
    INPUT_CLUSTERS,
    MODEL,
    MODELS_INFO,
    OUTPUT_CLUSTERS,
    PROFILE_ID,
)
from zhaquirks.quirk_ids import TUYA_PLUG_ONOFF
from zhaquirks.tuya import (
    EnchantedDevice,
    TuyaNewManufCluster,
    TuyaZB1888Cluster,
    TuyaZBE000Cluster,
    TuyaZBElectricalMeasurement,
    TuyaZBExternalSwitchTypeCluster,
    TuyaZBMeteringCluster,
    TuyaZBMeteringClusterWithUnit,
    TuyaZBOnOffAttributeCluster,
)

class Plug_v2l(EnchantedDevice):
    """Another TS011F Tuya plug. First one using this definition is _TZ3000_okaz9tjs."""

    quirk_id = TUYA_PLUG_ONOFF

    signature = {
        MODEL: "TS011F",
        ENDPOINTS: {
            # "profile_id": 260,
            # "device_type": "0x0100",
            # "in_clusters": ["0x0000", "0x0003", "0x0004", "0x0005", "0x0006", "0x0702", "0x0b04", "0xe001"],
            # "in_clusters": ["0x0000", "0x0003", "0x0004", "0x0005", "0x0006", "0x000a", "0x0702", "0x0b04", "0x1000", "0xe000", "0xe001"],
            # "out_clusters": []
            1: {
                PROFILE_ID: zha.PROFILE_ID,
                DEVICE_TYPE: zha.DeviceType.ON_OFF_LIGHT,
                INPUT_CLUSTERS: [
                    Basic.cluster_id,
                    Identify.cluster_id,
                    Groups.cluster_id,
                    Scenes.cluster_id,
                    OnOff.cluster_id,
                    Time.cluster_id,
                    Metering.cluster_id,
                    ElectricalMeasurement.cluster_id,
                    LightLink.cluster_id,
                    TuyaZBE000Cluster.cluster_id,
                    TuyaZBExternalSwitchTypeCluster.cluster_id,
                ],
                OUTPUT_CLUSTERS: [],
            },
        },
    }
    replacement = {
        ENDPOINTS: {
            1: {
                PROFILE_ID: zha.PROFILE_ID,
                DEVICE_TYPE: zha.DeviceType.SMART_PLUG,
                INPUT_CLUSTERS: [
                    Basic.cluster_id,
                    Identify.cluster_id,
                    Groups.cluster_id,
                    Scenes.cluster_id,
                    TuyaZBOnOffAttributeCluster,
                    Time.cluster_id,
                    TuyaZBMeteringClusterWithUnit,
                    TuyaZBElectricalMeasurement,
                    LightLink.cluster_id,
                    TuyaZBE000Cluster.cluster_id,
                    TuyaZBExternalSwitchTypeCluster,
                ],
                OUTPUT_CLUSTERS: [],
            },
        },
    }
