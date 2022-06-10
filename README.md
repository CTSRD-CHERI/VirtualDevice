# VirtualDevice
A Bluespec AXI component that is intended to act as a virtual device with a "device" interface and a management interface.

The device interface simply blocks on any request waiting for the management interface to specify and trigger a response.
The assumption is that you have both a CPU that is master to this virtual device, and an independent CPU providing the virtualisation of the device which is exercising the management interface.
