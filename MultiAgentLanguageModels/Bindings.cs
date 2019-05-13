using Ninject.Modules;
using MultiAgentLanguageModels;

public class Bindings : NinjectModule
{
    public override void Load()
    {
        Bind<IPrologService>().To<PrologService>();
    }
}