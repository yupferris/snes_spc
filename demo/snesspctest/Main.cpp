#include <Fsl.h>
using namespace Fsl;

#include <Fel.h>
using namespace Fel;

#include "../snes_spc/spc.h"

#include <Windows.h>

class OutputDevice
{
public:
	OutputDevice(SNES_SPC *apu, SPC_Filter *filter, IAudioDriver *driver)
	{
		this->apu = apu;
		this->filter = filter;
		this->driver = driver;

		stereoBuffer = new short[driver->GetSampleRate() * 2];

		mutex = Mutex::Create();

		driver->SetRenderCallback(renderCallbackRouter, this);
	}

	~OutputDevice()
	{
		driver->SetRenderCallback(nullptr, nullptr);

		delete [] stereoBuffer;

		delete mutex;
	}

private:
	static void renderCallbackRouter(float *leftBuffer, float *rightBuffer, int numSamples, void *userData)
	{
		if (userData)
			((OutputDevice *)userData)->renderCallback(leftBuffer, rightBuffer, numSamples);
	}

	void renderCallback(float *leftBuffer, float *rightBuffer, int numSamples)
	{
		mutex->Lock();
		
		spc_play(apu, numSamples * 2, stereoBuffer);
		spc_filter_run(filter, stereoBuffer, numSamples * 2);

		for (int i = 0; i < numSamples; i++)
		{
			leftBuffer[i] = (float)stereoBuffer[i * 2] / 32768.0f;
			rightBuffer[i] = (float)stereoBuffer[i * 2 + 1] / 32768.0f;
		}

		mutex->Unlock();
	}

	SNES_SPC *apu;
	SPC_Filter *filter;
	IAudioDriver *driver;

	short *stereoBuffer;

	Mutex *mutex;
};

int Main(const List<String>& arguments)
{
	try
	{
		if (!arguments.Count())
			throw FSL_EXCEPTION("No input file specified");

		auto apu = spc_new();
		auto filter = spc_filter_new();

		auto spc = File::ReadAllBytes(arguments[0]);
		spc_load_spc(apu, spc.GetData(), spc.Count());
		spc_clear_echo(apu);
		spc_filter_clear(filter);

		auto driver = AudioDriverFactory::CreateDefault();
		driver->SetSampleRate(spc_sample_rate);

		auto device = new OutputDevice(apu, filter, driver);

		while (!GetAsyncKeyState(VK_ESCAPE))
			Threading::Sleep(5);

		delete device;

		delete driver;

		spc_filter_delete(filter);
		spc_delete(apu);
	}
	catch (const Exception& e)
	{
		Console::WriteLine("ERROR: " + e.GetMsg());
		return 1;
	}

	return 0;
}
