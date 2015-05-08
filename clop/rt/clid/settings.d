module clop.rt.clid.settings;

import std.string;

class Settings {
	public	{
		void setUseCPU()
		{
			_deviceType = "cpu";
		}

		void setUseGPU()
		{
			_deviceType = "gpu";
		}

		string deviceType()
		{
			return _deviceType;
		}
	}

	static Settings Instance()
	{
		static Settings s = null;
		if(s is null) {
			s = new Settings();
		}

		return s;
	}

	private {
		string _deviceType;

		this()
		{
			_deviceType = "cpu";
		}
	}
}