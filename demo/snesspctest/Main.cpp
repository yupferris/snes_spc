#include <Fsl.h>
using namespace Fsl;

#include <Fel.h>
using namespace Fel;

#include "../snes_spc/SNES_SPC.h"
#include "../snes_spc/SPC_Filter.h"

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
		
		apu->play(numSamples * 2, stereoBuffer);
		filter->run(stereoBuffer, numSamples * 2);

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

		auto apu = new SNES_SPC();
		auto filter = new SPC_Filter();

		auto spc = File::ReadAllBytes(arguments[0]);
		apu->load_spc(spc.GetData(), spc.Count());
		apu->clear_echo();
		filter->clear();

		Console::WriteLine("pre-render");
		const int preBufferSize = 0x10000;
		short preBuffer[preBufferSize * 2];
		const int preBufferTimes = 4;
		for (int i = 0; i < preBufferTimes; i++)
			apu->play(preBufferSize * 2, preBuffer);

		Console::WriteLine("render");
		auto driver = AudioDriverFactory::CreateDefault();
		driver->SetSampleRate(SNES_SPC::sample_rate);
		auto device = new OutputDevice(apu, filter, driver);

		while (!GetAsyncKeyState(VK_ESCAPE))
			Threading::Sleep(5);

		delete device;
		delete driver;

		delete filter;
		delete apu;
	}
	catch (const Exception& e)
	{
		Console::WriteLine("ERROR: " + e.GetMsg());
		return 1;
	}

	return 0;
}
